﻿module Program

open System.Net.Sockets

open StreamJsonRpc


[<EntryPoint>]
let main (args: string[]) =
    let port = int args.[0]

    let listener = new TcpListener(System.Net.IPAddress.Any, port)
    listener.Start();

    async {
        while true do
            use! tcpClient = listener.AcceptTcpClientAsync() |> Async.AwaitTask
            use networkStream = tcpClient.GetStream()

            use formatter = new SystemTextJsonFormatter()
            use handler = new NewLineDelimitedMessageHandler(networkStream, networkStream, formatter)
            formatter.JsonSerializerOptions.PropertyNamingPolicy <- Server.PascalCaseToSnakeCaseNamingPolicy()

            use jsonRpc = new JsonRpc(handler, Server.ElectrumProxyServer())
            
#if DEBUG
            jsonRpc.TraceSource.Listeners.Add(new System.Diagnostics.TextWriterTraceListener(System.Console.OpenStandardError()))
            |> ignore
            jsonRpc.TraceSource.Switch.Level <- System.Diagnostics.SourceLevels.All
#endif

            jsonRpc.Disconnected.Add(fun args -> 
                eprintfn "Disconnected. Reason=%A; Description=%A; Exception=%A" args.Reason args.Description args.Exception)

            jsonRpc.StartListening()
            do! jsonRpc.Completion |> Async.AwaitTask
    }
    |> Async.RunSynchronously

    0
