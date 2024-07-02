namespace GWallet.Backend.UtxoCoin

open GWallet.Backend

open NBitcoin
open Org.BouncyCastle.Crypto
open Org.BouncyCastle.Math


type SilentPaymentAddress = 
    {
        ScanPublicKey: PubKey
        SpendPublicKey: PubKey
    }

    static member MainNetPrefix = "sp"
    static member TestNetPrefix = "tsp"

    static member private GetEncoder(chainName: ChainName) =
        let hrp =
            if chainName = ChainName.Mainnet then
                SilentPaymentAddress.MainNetPrefix
            elif chainName = ChainName.Testnet then
                SilentPaymentAddress.TestNetPrefix
            else
                failwith "Only Mainnet and Testnet are supported for SilentPayment address"
        DataEncoders.Bech32Encoder(DataEncoders.ASCIIEncoder().DecodeData hrp)

    static member IsSilentPaymentAddress (address: string) =
        address.StartsWith SilentPaymentAddress.MainNetPrefix 
        || address.StartsWith SilentPaymentAddress.MainNetPrefix

    member self.Encode(network: Network) : string =
        let encoder = SilentPaymentAddress.GetEncoder network.ChainName
        let data = 
            let versionByte = 0uy // version 0
            Array.append
                [| versionByte |]
                (Array.append (self.ScanPublicKey.ToBytes()) (self.SpendPublicKey.ToBytes()))
        encoder.EncodeData(data, 0, data.Length, DataEncoders.Bech32EncodingType.BECH32M)

    static member Decode(encodedAddress: string) : SilentPaymentAddress =
        let chain =
            if encodedAddress.StartsWith SilentPaymentAddress.TestNetPrefix then
                ChainName.Testnet
            elif encodedAddress.StartsWith SilentPaymentAddress.MainNetPrefix then
                ChainName.Mainnet
            else
                failwith "Encoded SilentPayment address should start with tsp or sp"
        let encoder = SilentPaymentAddress.GetEncoder chain
        let data, _ = encoder.DecodeDataRaw(encodedAddress)
        let _versionByte = data.[0]
        // TODO: check data length
        let scanPubKeyBytes = data.[1..33]
        let spendPubKeyBytes = data.[34..67]
        {
            ScanPublicKey = PubKey scanPubKeyBytes
            SpendPublicKey = PubKey spendPubKeyBytes
        }

type SilentPaymentInput =
    | InvalidInput
    | InputForSharedSecretDerivation of ICoin * PubKey
    | InputJustForSpending of ICoin

module SilentPayments =
    let private scalarOrder = BigInteger("fffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141", 16)

    // see https://github.com/bitcoin/bips/blob/master/bip-0352.mediawiki#selecting-inputs
    let convertToSilentPaymentInput (coin: ICoin): SilentPaymentInput =
        let txOut = coin.TxOut
        let scriptPubKey = txOut.ScriptPubKey

        if scriptPubKey.IsScriptType ScriptType.P2PKH then
            match scriptPubKey.GetAllPubKeys() |> Array.tryHead with
            | Some pubKey when pubKey.IsCompressed -> InputForSharedSecretDerivation(coin, pubKey)
            | _ -> InputJustForSpending coin
        elif scriptPubKey.IsScriptType ScriptType.P2SH then
            let redeemScript = scriptPubKey.PaymentScript
            if redeemScript.IsScriptType ScriptType.P2WPKH then
                let witness = scriptPubKey.ToWitScript()
                let pubKey = PubKey(witness.[witness.PushCount - 1])
                if pubKey.IsCompressed then
                    InputForSharedSecretDerivation(coin, pubKey)
                else
                    InputJustForSpending coin
            else
                InputJustForSpending coin
        elif scriptPubKey.IsScriptType ScriptType.P2WPKH then
            let witness = scriptPubKey.ToWitScript()
            let pubKey = PubKey(witness.[witness.PushCount - 1])
            if pubKey.IsCompressed then
                InputForSharedSecretDerivation(coin, pubKey)
            else
                InputJustForSpending coin
        elif scriptPubKey.IsScriptType ScriptType.Taproot then
            match scriptPubKey.GetAllPubKeys() |> Array.tryHead with
            | Some pubKey when pubKey.IsCompressed -> 
                // TODO: check if internal key is H (need full transaction for that)
                InputForSharedSecretDerivation(coin, pubKey)
            | _ -> InputJustForSpending coin
        elif scriptPubKey.IsScriptType ScriptType.P2PK 
             || scriptPubKey.IsScriptType ScriptType.MultiSig
             || scriptPubKey.IsScriptType ScriptType.P2WSH then
            InputJustForSpending coin
        else
            InvalidInput

    let taggedHash (tag: string) (data: array<byte>) : array<byte> =
        let sha256 = Digests.Sha256Digest()
        
        let tag = System.Text.ASCIIEncoding.ASCII.GetBytes tag
        sha256.BlockUpdate(tag, 0, tag.Length)
        let tagHash = Array.zeroCreate<byte> 32
        sha256.DoFinal(tagHash, 0) |> ignore
        sha256.Reset()

        sha256.BlockUpdate(Array.append tagHash tagHash, 0, tagHash.Length * 2)
        sha256.BlockUpdate(data, 0, data.Length)

        let result = Array.zeroCreate<byte> 32
        sha256.DoFinal(result, 0) |> ignore
        result

    let getInputHash (outpoints: List<OutPoint>) (sumInputPubKeys: EC.ECPoint) : array<byte> =
        let lowestOutpoint = outpoints |> List.map (fun outpoint -> outpoint.ToBytes()) |> List.min
        let hashInput = Array.append lowestOutpoint (sumInputPubKeys.GetEncoded false)
        taggedHash "BIP0352/Inputs" hashInput

    let createOutput (privateKeys: List<Key * bool>) (outpoints: List<OutPoint>) (spAddress: SilentPaymentAddress) =
        if privateKeys.IsEmpty then
            failwith "privateKeys should not be empty"

        if outpoints.IsEmpty then
            failwith "outpoints should not be empty"

        let secp256k1 = EC.CustomNamedCurves.GetByName("secp256k1")

        let aSum = 
            privateKeys 
            |> List.map (
                fun (key, isTaproot) ->
                    let k = BigInteger(key.ToBytes())
                    let yCoord = secp256k1.Curve.DecodePoint(key.PubKey.ToBytes()).YCoord.ToBigInteger()
                    if isTaproot && yCoord.Mod(BigInteger.Two) = BigInteger.One then
                        k.Negate()
                    else
                        k)
            |> List.fold
                (fun (a: BigInteger) (b: BigInteger) -> a.Add b)
                BigInteger.Zero

        if aSum = BigInteger.Zero then
            failwith "Input privkeys sum is zero"

        let inputHash = getInputHash outpoints (secp256k1.G.Multiply aSum)

        let tweak = BigInteger inputHash;
        let tweakedSumSeckey = aSum.Multiply(tweak).Mod(scalarOrder)
        let sharedSecret = tweakedSumSeckey.Multiply(spAddress.ScanPublicKey.ToBytes() |> BigInteger).Mod(scalarOrder)

        let k = 0u
        let tK =
            taggedHash
                "BIP0352/SharedSecret"
                (Array.append (sharedSecret.ToByteArrayUnsigned()) (System.BitConverter.GetBytes k))
            |> BigInteger
        let sharedSecret = secp256k1.G.Multiply(BigInteger(spAddress.SpendPublicKey.ToBytes()).Add(tK))

        sharedSecret
