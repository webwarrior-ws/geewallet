module Program

open System.Net.Sockets

open StreamJsonRpc



[<EntryPoint>]
let main (args: string[]) =
    let port = int args.[0]

    let listener = new TcpListener(System.Net.IPAddress.Any, port)
    listener.Start();
    
    use tcpClient = listener.AcceptTcpClient();
    use networkStream = tcpClient.GetStream()

    use formatter = new SystemTextJsonFormatter()
    use handler = new NewLineDelimitedMessageHandler(networkStream, networkStream, formatter)
    formatter.JsonSerializerOptions.PropertyNamingPolicy <- Server.PascalCaseToSnakeCaseNamingPolicy()

    async {
        while true do
            use jsonRpc = new JsonRpc(handler)
            
            Server.AddMethods jsonRpc

            jsonRpc.StartListening()
            do! jsonRpc.Completion |> Async.AwaitTask
    }
    |> Async.RunSynchronously

    0
