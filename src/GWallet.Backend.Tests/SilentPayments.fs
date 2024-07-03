namespace GWallet.Backend.Tests

open System.IO
open System.Reflection
open System.Text.Json

open NUnit.Framework
open NBitcoin

open GWallet.Backend
open GWallet.Backend.UtxoCoin
open GWallet.Backend.UtxoCoin.SilentPayments


type private TestInput =
    {
        TxId: string
        Vout: int
        ScriptSig: string
        TxInWitness: string
        ScriptPubKey: string
        PrivateKey: string
    }
    static member FromJsonElement(jsonElement: JsonElement) =
        {
            TxId = jsonElement.GetProperty("txid").GetString()
            Vout = jsonElement.GetProperty("vout").GetInt32()
            ScriptSig = jsonElement.GetProperty("scriptSig").GetString()
            TxInWitness = jsonElement.GetProperty("txinwitness").GetString()
            ScriptPubKey = jsonElement.GetProperty("prevout").GetProperty("scriptPubKey").GetProperty("hex").GetString()
            PrivateKey = jsonElement.GetProperty("private_key").GetString()
        }

[<TestFixture>]
type SilentPayments() =
    
    let executingAssembly = Assembly.GetExecutingAssembly()
    let binPath = executingAssembly.Location |> FileInfo
    let projectDirPath = Path.Combine(binPath.Directory.FullName, "..", "..", "..")
    let dataDir = Path.Combine(projectDirPath, "data") |> DirectoryInfo

    [<Test>]
    member __.``Test creating outputs using test vectors from BIP-352``() =
        // https://github.com/bitcoin/bips/blob/master/bip-0352/send_and_receive_test_vectors.json

        let testVectorsFileName = "send_and_receive_test_vectors.json"
        let testVectorsJson = JsonDocument.Parse(File.ReadAllText(Path.Combine(dataDir.FullName, testVectorsFileName)))

        for testCase in testVectorsJson.RootElement.EnumerateArray() do
            let name = testCase.GetProperty "comment"
            let sending = testCase.GetProperty("sending").[0]
            let expectedOutputs = 
                sending.GetProperty("expected").GetProperty("outputs").EnumerateArray() 
                    |> Seq.map (fun each -> each.EnumerateArray() |> Seq.toArray)
                    |> Seq.toArray
            
            if expectedOutputs.Length > 1 || (expectedOutputs.Length = 1 && expectedOutputs.[0].Length > 1) then
                () // skip
            else
                let expectedOutput = expectedOutputs.[0] |> Array.tryHead |> Option.map (fun elem -> elem.GetString())
                let given = sending.GetProperty "given"
                let inputs = given.GetProperty("vin").EnumerateArray() |> Seq.map TestInput.FromJsonElement |> Seq.toArray
                let recipients = 
                    given.GetProperty("recipients").EnumerateArray()
                        |> Seq.map (fun each -> each.GetString() |> SilentPaymentAddress.Decode)
                        |> Seq.toArray

                ignore (name, expectedOutput, inputs, recipients)
