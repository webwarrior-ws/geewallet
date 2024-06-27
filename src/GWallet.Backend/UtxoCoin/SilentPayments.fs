﻿namespace GWallet.Backend.UtxoCoin

open GWallet.Backend

open NBitcoin


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

    member self.Encode(network: Network) : string =
        let encoder = SilentPaymentAddress.GetEncoder network.ChainName
        let data = 
            let versionByte = byte 'q' // version 0
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


module SilentPayments =
    ()
