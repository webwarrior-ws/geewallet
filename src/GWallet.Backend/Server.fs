namespace GWallet.Backend

open System

type ExceptionInfo =
    { TypeFullName: string
      Message: string }

type FaultInfo =
    {
        Exception: ExceptionInfo
        LastSuccessfulCommunication: Option<DateTime>
    }

type Status =
    | Fault of FaultInfo
    | Success

type HistoryInfo =
    { TimeSpan: TimeSpan
      Status: Status }

type Protocol =
    | Http
    | Tcp of port: uint32

type ConnectionType =
    {
        Encrypted: bool
        Protocol: Protocol
    }

type ICommunicationHistory =
    abstract member CommunicationHistory: Option<HistoryInfo> with get

type HistoryFact =
    {
        TimeSpan: TimeSpan
        Fault: Option<ExceptionInfo>
    }

type ServerInfo =
    {
        NetworkPath: string
        ConnectionType: ConnectionType
    }

// FIXME: create type 'CurrencyServer' that is a record of Currency*ServerDetails which is rather used instead
// of ServerDetails, so that functions that use a server also know which currency they're dealing with (this
// way we can, for example, retry if NoneAvailable exception in case ETC is used, cause there's a lack of servers
// in that ecosystem at the moment)

[<CustomEquality; NoComparison>]
type ServerDetails =
    {
        ServerInfo: ServerInfo
        CommunicationHistory: Option<CachedValue<HistoryInfo>>
    }
    member private self.EqualsInternal (yObj: obj) =
        match yObj with
        | :? ServerDetails as y ->
            self.ServerInfo.Equals y.ServerInfo
        | _ -> false
    override self.Equals yObj =
        self.EqualsInternal yObj
    override self.GetHashCode () =
        self.ServerInfo.GetHashCode()
    interface ICommunicationHistory with
        member self.CommunicationHistory
            with get() =
                match self.CommunicationHistory with
                | None -> None
                | Some (h,_) -> Some h

type ServerRanking = 
    private {
        Ranking: Map<Currency, array<ServerDetails>>
    }
    with
        static member Empty = { Ranking = Map.empty }

        member self.Item with get(currency: Currency) = self.Ranking.[currency] :> seq<ServerDetails>

        member self.TryGetForCurrency(currency: Currency) = 
            self.Ranking.TryFind currency 
            |> Option.map(fun arr -> arr :> seq<ServerDetails>)

        interface Collections.Generic.IEnumerable<Collections.Generic.KeyValuePair<Currency, seq<ServerDetails>>> with
            member self.GetEnumerator(): Collections.Generic.IEnumerator<Collections.Generic.KeyValuePair<Currency,ServerDetails seq>> = 
                (seq { for KeyValue(k, v) in self.Ranking -> Collections.Generic.KeyValuePair(k, v :> seq<ServerDetails>) }).GetEnumerator()

            member self.GetEnumerator(): Collections.IEnumerator = 
                (self :> Collections.Generic.IEnumerable<_>).GetEnumerator()

        member internal self.TryFindValue (serverPredicate: ServerDetails -> bool) : Option<Currency*ServerDetails> =
            self.Ranking 
            |> Map.tryPick 
                (fun currency servers -> 
                    servers 
                    |> Array.tryFind serverPredicate
                    |> Option.map (fun server -> (currency, server)) )
        
        member private self.IsBlackListed currency server =
            // as these servers can only serve very limited set of queries (e.g. only balance?) their stats are skewed and
            // they create exception when being queried for advanced ones (e.g. latest block)
            server.ServerInfo.NetworkPath.Contains "blockscout" ||

            // there was a mistake when adding this server to our servers.json file: it was added in the ETC currency instead of ETH
            (currency = Currency.ETC && server.ServerInfo.NetworkPath.Contains "ethrpc.mewapi.io")

            // there was a typo when adding this server to our servers.json file, see commit 69d90fd2fc22a1f3dd9ef8793f0cd42e3b540df1
            || (currency = Currency.ETC && server.ServerInfo.NetworkPath.Contains "ethercluster.comx/")

        member self.AddServer (currency: Currency) (newServer: ServerDetails) : ServerRanking =
            if self.IsBlackListed currency newServer then
                self
            else
                let servers = 
                    match self.Ranking.TryFind currency with
                    | Some value -> value
                    | None -> Array.empty

                let updatedServers =
                    match Array.tryFindIndex (fun each -> each.ServerInfo.NetworkPath = newServer.ServerInfo.NetworkPath) servers with
                    | Some index ->
                        let existingServer = servers.[index]
                        match newServer.CommunicationHistory, existingServer.CommunicationHistory with
                        | None, _ -> ()
                        | _, None -> 
                            servers.[index] <- newServer
                        | Some (_, newLastComm),Some (_, existingLastComm) when newLastComm > existingLastComm ->
                            servers.[index] <- newServer
                        | _ -> ()
                        servers
                    | None -> 
                        Array.append servers [| newServer |]

                { Ranking = self.Ranking |> Map.add currency updatedServers }

        member self.AddAllServers (currency: Currency) (newServers: seq<ServerDetails>) : ServerRanking =
            Seq.fold
                (fun currRanking server -> currRanking.AddServer currency server)
                self
                newServers

        member self.Merge(other: ServerRanking) : ServerRanking =
            Seq.fold
                (fun ranking (currency, servers) -> ranking.AddAllServers currency servers)
                self
                (other.Ranking |> Map.toSeq)

        member self.Sorted() : ServerRanking =
            let sort server =
                let invertOrder (timeSpan: TimeSpan): int =
                    0 - int timeSpan.TotalMilliseconds
                match server.CommunicationHistory with
                | None -> None
                | Some (history, lastComm) ->
                    match history.Status with
                    | Fault faultInfo ->
                        let success = false
                        match faultInfo.LastSuccessfulCommunication with
                        | None -> Some (success, invertOrder history.TimeSpan, None)
                        | Some lsc -> Some (success, invertOrder history.TimeSpan, Some lsc)
                    | Success ->
                        let success = true
                        Some (success, invertOrder history.TimeSpan, Some lastComm)

            { Ranking = self.Ranking |> Map.map (fun _ servers -> Array.sortByDescending sort servers) }

        member self.Serialize(): string =
            self.Sorted().Ranking
            |> Map.map (fun _ servers -> servers |> Array.toSeq)
            |> Marshalling.Serialize

        static member Deserialize(json: string): ServerRanking =
            { Ranking = Marshalling.Deserialize json |> Map.map (fun _ servers -> servers |> Array.ofSeq) }

module ServerRegistry =

    let ServersEmbeddedResourceFileName = "servers.json"

    let Serialize(servers: ServerRanking): string =
        servers.Serialize()

    let Deserialize(json: string): ServerRanking =
        ServerRanking.Deserialize json

    let private ServersRankingBaseline =
        Deserialize (Fsdk.Misc.ExtractEmbeddedResourceFileContents ServersEmbeddedResourceFileName)

    let MergeWithBaseline (ranking: ServerRanking): ServerRanking =
        ranking.Merge ServersRankingBaseline

[<CustomEquality; NoComparison>]
type Server<'K,'R when 'K: equality and 'K :> ICommunicationHistory> =
    { Details: 'K
      Retrieval: Async<'R> }
    override self.Equals yObj =
        match yObj with
        | :? Server<'K,'R> as y ->
            self.Details.Equals y.Details
        | _ -> false
    override self.GetHashCode () =
        self.Details.GetHashCode()
