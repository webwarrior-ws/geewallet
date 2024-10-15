module BitcoreNodeClient

open System
open System.Net.Http
open System.Text.Json

open GWallet.Backend
open GWallet.Backend.UtxoCoin


// https://github.com/bitpay/bitcore/blob/master/packages/bitcore-node/docs/api-documentation.md
type BitcoreNodeClient(serverAddress: string) =
    let httpClient = new HttpClient(BaseAddress=Uri serverAddress)

    interface IDisposable with
        override self.Dispose (): unit = 
            httpClient.Dispose()
    
    member private self.Request(request: string): Async<string> =
        async {
            try
                return! httpClient.GetStringAsync request |> Async.AwaitTask
            with
            | :? HttpRequestException as ex ->
                // maybe only discard server on several specific errors?
                let msg = sprintf "%s: %s" (ex.GetType().FullName) ex.Message
                return raise <| ServerDiscardedException(msg, ex)
            | :? Threading.Tasks.TaskCanceledException as ex ->
                let msg = sprintf "Timeout: %s" ex.Message
                return raise <| ServerDiscardedException(msg, ex)
        }

    member self.GetAddressTransactions(address: string): Async<array<BlockchainScriptHashGetHistoryInnerResult>> =
        async {
            let request = $"/api/BTC/mainnet/address/{address}/txs"
            let! response = self.Request request
            let json = JsonDocument.Parse response
            return [| for entry in json.RootElement.EnumerateArray() -> 
                        { TxHash = entry.GetProperty("mintTxid").GetString(); 
                          Height = entry.GetProperty("mintHeight").GetUInt64() } |]
        }
