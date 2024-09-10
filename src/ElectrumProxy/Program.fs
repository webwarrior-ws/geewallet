module Program

open System
open System.Text
open System.Net.Sockets

open StreamJsonRpc

let supportedProtocolVersion = "0.10"

type PascalCaseToSnakeCaseNamingPolicy() = 
    inherit Json.JsonNamingPolicy()

    static let capitalizedWordRegex = RegularExpressions.Regex "[A-Z][a-z0-9]*"

    override self.ConvertName name =
        let evaluator (regexMatch: RegularExpressions.Match) =
            let lowercase = regexMatch.Value.ToLower()
            if regexMatch.Index = 0 then lowercase else "_" + lowercase
        capitalizedWordRegex.Replace(name, Text.RegularExpressions.MatchEvaluator evaluator)

[<EntryPoint>]
let main (args: string[]) =
    let port = int args.[0]

    let listener = new TcpListener(System.Net.IPAddress.Any, port)
    listener.Start();
    
    use tcpClient = listener.AcceptTcpClient();
    use networkStream = tcpClient.GetStream()

    use formatter = new SystemTextJsonFormatter()
    use handler = new NewLineDelimitedMessageHandler(networkStream, networkStream, formatter)
    formatter.JsonSerializerOptions.PropertyNamingPolicy <- PascalCaseToSnakeCaseNamingPolicy()

    async {
        while true do
            use jsonRpc = new JsonRpc(handler)
            
            jsonRpc.AddLocalRpcMethod(
                "server.version", 
                new Func<string, string, string>(fun _clientVersion _protocolVersion -> supportedProtocolVersion))

            jsonRpc.StartListening()
            do! jsonRpc.Completion |> Async.AwaitTask
    }
    |> Async.RunSynchronously

    0
