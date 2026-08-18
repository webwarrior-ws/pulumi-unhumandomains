namespace Pulumi.UnhumanDomains

open System
open System.Collections.Immutable
open System.Collections.Generic
open System.Linq
open System.Net
open System.Net.Http
open System.Threading
open System.Threading.Tasks
open System.Text.Json

open Pulumi
open Pulumi.Experimental
open Pulumi.Experimental.Provider

type UnhumanDomainsProvider(host: Pulumi.Experimental.IEngine) =
    inherit Pulumi.Experimental.Provider.Provider()

    let mutable internalHttpClient:Option<HttpClient> = None

    static let domainRecordResourceName = "unhumandomains:index:Domain"
    static let apiBaseUrl = "https://unhuman.domains"

    let recordEquivalentTo (rec1: IDictionary<string,obj>) (rec2: IDictionary<string,obj>) =
        rec1.["type"] = rec2["type"] && rec1.["subdomain"] = rec2.["subdomain"]

    let primitivePropertyValueToObject (value: PropertyValue): obj =
        if value.Type = PropertyValueType.String then
            let _, strValue = value.TryGetString()
            box strValue
        elif value.Type = PropertyValueType.Number then
            let _, numValue = value.TryGetNumber()
            box numValue
        else
            failwithf "Unexpected property value type: %A" value.Type

    let recordPropertyValueToDictionary (recordPropertyValue: PropertyValue) : IDictionary<string, obj> =
        let _, object = recordPropertyValue.TryGetMap()
        object 
        |> Seq.map (fun item -> item.Key, primitivePropertyValueToObject item.Value)
        |> dict

    member this.HttpClient =
        match internalHttpClient with
        | None ->
            failwith "Provider arg not passed! Required to receive token via provider arg since version 0.0.7"
        | Some httpClient ->
            httpClient

    // Provider has to advertise its version when outputting schema, e.g. for SDK generation.
    // In pulumi-bitlaunch, we have Pulumi generate the terraform bridge, and it automatically pulls version from the tag.
    // Use sdk/dotnet/version.txt as source of version number.
    // WARNING: that file is deleted when SDK is generated using `pulumi package gen-sdk` command; it has to be re-created.
    static member val Version = 
        let assembly = System.Reflection.Assembly.GetExecutingAssembly()
        let resourceName = 
            assembly.GetManifestResourceNames()
            |> Seq.find (fun str -> str.EndsWith "version.txt")
        use stream = assembly.GetManifestResourceStream resourceName
        use reader = new System.IO.StreamReader(stream)
        reader.ReadToEnd().Trim()

    interface IDisposable with
        override this.Dispose (): unit =
            match internalHttpClient with
            | Some httpClient ->
                httpClient.Dispose()
            | None ->
                ()
    
    member private this.AsyncGetDnsRecords(domainName: string): Async<Option<seq<IDictionary<string, PropertyValue>>>> =
        async {
            do! 
                host.LogAsync(LogRequest(LogSeverity.Info, $"[UnhumanDomains] GET /api/domains/{domainName}/dns"))
                |> Async.AwaitTask
            let! dnsRecordsResponse = 
                this.HttpClient.GetAsync($"{apiBaseUrl}/api/domains/{domainName}/dns")
                |> Async.AwaitTask
            let! responseBody = dnsRecordsResponse.Content.ReadAsStringAsync() |> Async.AwaitTask
            do! 
                host.LogAsync(LogRequest(
                    LogSeverity.Info,
                    $"[UnhumanDomains] GET /api/domains/{domainName}/dns <- {(int)dnsRecordsResponse.StatusCode} {responseBody}")
                )
                |> Async.AwaitTask
            if dnsRecordsResponse.StatusCode = HttpStatusCode.Conflict then
                // custom nameservers
                return None
            elif not dnsRecordsResponse.IsSuccessStatusCode then
                return failwith $"Error getting DNS records (status code {dnsRecordsResponse.StatusCode}):
{responseBody}"
            else
                let data = JsonDocument.Parse(responseBody).RootElement.GetProperty "data"
                let records = 
                    data.GetProperty("records").EnumerateArray()
                    |> Seq.map 
                        (fun elem -> 
                            elem.EnumerateObject() 
                            |> Seq.map(fun object -> 
                                let value = 
                                    if object.Value.ValueKind = JsonValueKind.String then
                                        object.Value.GetString() |> PropertyValue
                                    elif object.Value.ValueKind = JsonValueKind.Number then
                                        object.Value.GetInt32() |> PropertyValue
                                    else
                                        failwithf 
                                            "Unexpected value kind of property %s: %A"
                                                object.Name
                                                object.Value.ValueKind
                                object.Name, value)
                            |> dict)
                return Some records
        }

    member private this.AsyncGetNameservers(domainName: string): Async<seq<string>> =
        async {
            let! domainInfo = 
                this.HttpClient.GetStringAsync($"{apiBaseUrl}/api/domains/{domainName}/info")
                |> Async.AwaitTask
            let data = JsonDocument.Parse(domainInfo).RootElement.GetProperty "data"
            let nameservers = 
                data.GetProperty("nameservers").EnumerateArray()
                |> Seq.map (fun elem -> elem.GetString())
            return nameservers
        }

    member private this.AsyncSetDefaultNameservers (domainName: string): Async<unit> =
        async {
            do! 
                host.LogAsync(LogRequest(
                    LogSeverity.Info,
                    $"[UnhumanDomains] PUT /api/domains/{domainName}/nameservers (default)")
                )
                |> Async.AwaitTask
            let! useDefaultNameserversResponse = 
                this.HttpClient.PutAsync(
                    $"{apiBaseUrl}/api/domains/{domainName}/nameservers",
                    Json.JsonContent.Create {| useDefault = true |}
                )
                |> Async.AwaitTask
            let! responseBody = useDefaultNameserversResponse.Content.ReadAsStringAsync() |> Async.AwaitTask
            do! 
                host.LogAsync(LogRequest(
                    LogSeverity.Info, 
                    $"[UnhumanDomains] PUT /api/domains/{domainName}/nameservers <- {(int)useDefaultNameserversResponse.StatusCode} {responseBody}")
                )
                |> Async.AwaitTask
            if not useDefaultNameserversResponse.IsSuccessStatusCode then
                return failwith $"Error swithching to default nameservers (status code {useDefaultNameserversResponse.StatusCode}):
{responseBody}"
        }

    member private this.AsyncSetDnsRecords
        (domainName: string)
        (recordsToSet: seq<IDictionary<string,obj>>)
        (recordsToDelete: seq<IDictionary<string,obj>>)
        : Async<unit> =
        async {
            // first make sure that default nameservers are used
            do! this.AsyncSetDefaultNameservers domainName

            // get existing records first to not lose them when updating
            let! maybeExistingRecords = this.AsyncGetDnsRecords domainName
            let updatedRecords =
                match maybeExistingRecords with
                | Some existingRecords ->
                    let transformedExisting =
                        existingRecords
                        |> Seq.map (fun record ->
                            record
                            |> Seq.map (fun item -> item.Key, primitivePropertyValueToObject item.Value)
                            |> dict)
                    let keptExisting =
                        seq {
                            for existingRecord in transformedExisting do
                                if recordsToDelete |> Seq.exists (recordEquivalentTo existingRecord) then
                                    ()
                                else
                                    match recordsToSet |> Seq.tryFind (recordEquivalentTo existingRecord) with
                                    | Some newRecord -> yield newRecord
                                    | None -> yield existingRecord
                        }
                    let newRecords =
                        recordsToSet
                        |> Seq.filter (fun newRecord ->
                            not (transformedExisting |> Seq.exists (recordEquivalentTo newRecord)))
                    Seq.append keptExisting newRecords
                | None -> recordsToSet

            // Don't set 'records' array to empty because unhumandomains will give an error
            // even though 'records' field is present and contains empty array ([]).
            if Seq.isEmpty updatedRecords then
                do! 
                    host.LogAsync(LogRequest(
                        LogSeverity.Info,
                        $"[UnhumanDomains] 'records' array is empty, skipping PUT /api/domains/{domainName}/dns"))
                    |> Async.AwaitTask
                return ()

            let payload = {| records = updatedRecords |}
            let payloadJson = JsonSerializer.Serialize payload
            do! 
                host.LogAsync(LogRequest(LogSeverity.Info, $"[UnhumanDomains] PUT /api/domains/{domainName}/dns -> {payloadJson}"))
                |> Async.AwaitTask
            let! putDnsRecordsResponse = 
                this.HttpClient.PutAsync($"{apiBaseUrl}/api/domains/{domainName}/dns", Json.JsonContent.Create payload)
                |> Async.AwaitTask
            let! responseBody = putDnsRecordsResponse.Content.ReadAsStringAsync() |> Async.AwaitTask
            do! 
                host.LogAsync(LogRequest(
                    LogSeverity.Info,
                    $"[UnhumanDomains] PUT /api/domains/{domainName}/dns <- {(int)putDnsRecordsResponse.StatusCode} {responseBody}")
                )
                |> Async.AwaitTask
            if not putDnsRecordsResponse.IsSuccessStatusCode then
                let! responseBody = putDnsRecordsResponse.Content.ReadAsStringAsync() |> Async.AwaitTask
                return failwith $"Error setting DNS records (status code {putDnsRecordsResponse.StatusCode}):
{responseBody}"
        }

    member private this.AsyncSetNameservers (domainName: string) (nameservers: seq<string>): Async<unit> =
        async {
            let payload = {| nameservers = nameservers |}
            let payloadJson = JsonSerializer.Serialize payload
            do! 
                host.LogAsync(LogRequest(
                    LogSeverity.Info,
                    $"[UnhumanDomains] PUT /api/domains/{domainName}/nameservers -> {payloadJson}"))
                |> Async.AwaitTask
            let! putNameserversResponse = 
                this.HttpClient.PutAsync($"{apiBaseUrl}/api/domains/{domainName}/nameservers", Json.JsonContent.Create payload)
                |> Async.AwaitTask
            let! responseBody = putNameserversResponse.Content.ReadAsStringAsync() |> Async.AwaitTask
            do! 
                host.LogAsync(LogRequest(
                    LogSeverity.Info,
                    $"[UnhumanDomains] PUT /api/domains/{domainName}/nameservers <- {(int)putNameserversResponse.StatusCode} {responseBody}"))
                |> Async.AwaitTask
            if not putNameserversResponse.IsSuccessStatusCode then
                let! responseBody = putNameserversResponse.Content.ReadAsStringAsync() |> Async.AwaitTask
                return failwith $"Error setting nameservers (status code {putNameserversResponse.StatusCode}):
{responseBody}"
        }

    member private self.AsyncCreateOrUpdate
        (properties: ImmutableDictionary<string, PropertyValue>)
        (oldProperties: ImmutableDictionary<string, PropertyValue>) =
        async {
            let _, domainName = properties.["domainName"].TryGetString()
            let hasDnsRecords, dnsRecordsPropertyValue = properties.TryGetValue "records"
            let hasNameservers, nameserversPropertyValue = properties.TryGetValue "nameservers"
                
            match hasDnsRecords, hasNameservers with
            | true, false ->
                let _, recordsArray = 
                    dnsRecordsPropertyValue.TryGetArray()
                let recordsToSet =
                    recordsArray
                    |> Seq.map recordPropertyValueToDictionary
                    |> Seq.cache
                let recordsToDelete =
                    let oldRecords =
                        match oldProperties.TryGetValue "records" with
                        | true, recordsValue -> 
                            let _, records = recordsValue.TryGetArray()
                            records
                        | false, _ -> ImmutableArray.Empty
                    oldRecords 
                    |> Seq.map recordPropertyValueToDictionary
                    |> Seq.filter (fun record -> not (recordsToSet |> Seq.exists (recordEquivalentTo record)))
                    |> Seq.cache
                do! self.AsyncSetDnsRecords domainName recordsToSet recordsToDelete
            | false, true ->
                let _, nameserversArray = nameserversPropertyValue.TryGetArray()
                let nameservers = 
                    nameserversArray
                    |> Seq.map (fun nameserver -> nameserver.TryGetString() |> snd)
                do! self.AsyncSetNameservers domainName nameservers                    
            | _ -> return failwith "Unreachable"
        }

    override self.GetSchema (request: GetSchemaRequest, ct: CancellationToken): Task<GetSchemaResponse> = 
        let schema =
            let dnsRecordTypes = """{
                        "unhumandomains:index:DnsRecordType": {
                            "type": "string",
                            "enum": [ 
                                { "value": "A" },
                                { "value": "AAAA" },
                                { "value": "CNAME" },
                                { "value": "TXT" },
                                { "value": "MX" }
                            ]
                        },
                        "unhumandomains:index:DnsRecord": { 
                            "type": "object",
                            "properties": {
                                "type": {
                                    "type": "string",
                                    "$ref": "#/types/unhumandomains:index:DnsRecordType"
                                },
                                "subdomain": {
                                    "type": "string"
                                },
                                "ip": {
                                    "type": "string"
                                },
                                "target": {
                                    "type": "string"
                                },
                                "value": {
                                    "type": "string"
                                },
                                "priority": {
                                    "type": "number"
                                }
                            },
                            "required": [ "type", "subdomain" ]
                        }
                    }"""
            
            let domainRecordProperties = 
                """{
                                "domainName": {
                                    "type": "string"
                                },
                                "records": {
                                    "type": "array",
                                    "items": {
                                        "type": "object",
                                        "$ref": "#/types/unhumandomains:index:DnsRecord"
                                    }
                                },
                                "nameservers": {
                                    "type": "array",
                                    "items": {
                                        "type": "string"
                                    }
                                }
                            }"""

            sprintf
                """{
                    "name": "unhumandomains",
                    "version": "%s",
                    "resources": {
                        "%s" : {
                            "properties": %s,
                            "inputProperties": %s,
                            "requiredInputs": [ "domainName" ]
                        }
                    },
                    "types": %s,
                    "provider": {
                        "inputProperties": {
                            "apiToken": {
                                "type": "string"
                            }
                        },
                        "requiredInputs": [ "apiToken" ]
                    }
                }"""
                UnhumanDomainsProvider.Version
                domainRecordResourceName
                domainRecordProperties
                domainRecordProperties
                dnsRecordTypes

        Task.FromResult <| GetSchemaResponse(Schema = schema)

    override self.CheckConfig (request: CheckRequest, ct: CancellationToken): Task<CheckResponse> = 
        Task.FromResult <| CheckResponse(Inputs = request.NewInputs)

    override self.DiffConfig (request: DiffRequest, ct: CancellationToken): Task<DiffResponse> = 
        Task.FromResult <| DiffResponse()

    override self.Configure (request: ConfigureRequest, ct: CancellationToken): Task<ConfigureResponse> =
        let maybeApiTokenFromProviderArgs =
            match request.Args.TryGetValue "apiToken" with
            | true, value ->
                match value.TryGetString() with
                | true, token -> Some token
                | false, _ -> None
            | false, _ -> None

        match maybeApiTokenFromProviderArgs with
        | Some apiToken ->
            match internalHttpClient with
            | Some _ ->
                failwith "Pulumi deployment should not call this.Configure() more than once?"
            | None ->
                let newHttpClient = new HttpClient()
                newHttpClient.DefaultRequestHeaders.Authorization <- Headers.AuthenticationHeaderValue("Bearer", apiToken)
                internalHttpClient <- Some newHttpClient
        | None ->
            ()

        Task.FromResult <| ConfigureResponse()
   
    override self.Check (request: CheckRequest, ct: CancellationToken): Task<CheckResponse> = 
        if request.Type = domainRecordResourceName then 
            let hasDnsRecords, dnsRecords = request.NewInputs.TryGetValue "records"
            let hasNameservers = request.NewInputs.ContainsKey "nameservers"
            let failures = 
                match hasDnsRecords, hasNameservers with
                | true, true | false, false ->
                    CheckFailure("records|nameservers", "Either 'records' or 'nameservers' must be specified, but not both") 
                    |> Array.singleton
                | true, false ->
                    match dnsRecords.TryGetArray() with
                    | true, dnsRecordsArray ->
                        dnsRecordsArray
                        |> Seq.collect (fun dnsRecord ->
                            match dnsRecord.TryGetMap() with
                            | true, dnsRecordsObject ->
                                match dnsRecordsObject.TryGetValue "type" with
                                | true, typ when typ.TryGetString() = (true, "A") || typ.TryGetString() = (true, "AAAA") ->
                                    if not <| dnsRecordsObject.ContainsKey "ip" then
                                        CheckFailure("records", "A/AAAA records must have 'ip' property") |> Array.singleton
                                    else
                                        Array.empty
                                | true, typ when typ.TryGetString() = (true, "CNAME") ->
                                    if not <| dnsRecordsObject.ContainsKey "target" then
                                        CheckFailure("records", "CNAME records must have 'target' property") |> Array.singleton
                                    else
                                        Array.empty
                                | true, _ ->
                                    Array.empty
                                | false, _ ->
                                    CheckFailure("records", "DNS record must have 'type' property") |> Array.singleton
                            | false, _ ->
                                CheckFailure("records", "Item of 'records' array must be an object") 
                                |> Array.singleton
                        )
                        |> Seq.toArray
                    | false, _ ->
                        CheckFailure("records", "'records' must be an array") 
                        |> Array.singleton
                | false, true ->
                    Array.empty
            Task.FromResult <| CheckResponse(Inputs = request.NewInputs, Failures = failures)
        else
            failwith $"Unknown resource type '{request.Type}'"

    override self.Diff (request: DiffRequest, ct: CancellationToken): Task<DiffResponse> = 
        if request.Type = domainRecordResourceName then
            let diff = request.NewInputs.Except request.OldInputs 
            let replaces = diff |> Seq.map (fun pair -> pair.Key) |> Seq.toArray
            Task.FromResult <| DiffResponse(Changes = (replaces.Length > 0), Replaces = replaces)
        else
            failwith $"Unknown resource type '{request.Type}'"

    member private self.AsyncCreate(request: CreateRequest): Async<CreateResponse> =
        async {
            if request.Type = domainRecordResourceName then
                do! self.AsyncCreateOrUpdate request.Properties ImmutableDictionary.Empty
                return CreateResponse(Id = Guid.NewGuid().ToString(), Properties = request.Properties)
            else
                return failwith $"Unknown resource type '{request.Type}'"
        }

    override self.Create (request: CreateRequest, ct: CancellationToken): Task<CreateResponse> = 
        Async.StartAsTask(self.AsyncCreate request, TaskCreationOptions.None, ct)

    member private self.AsyncUpdate(request: UpdateRequest): Async<UpdateResponse> =
        async {
            if request.Type = domainRecordResourceName then
                do! self.AsyncCreateOrUpdate request.News request.Olds
                return UpdateResponse(Properties = request.News)
            else
                return failwith $"Unknown resource type '{request.Type}'"
        }

    override self.Update (request: UpdateRequest, ct: CancellationToken): Task<UpdateResponse> = 
        Async.StartAsTask(self.AsyncUpdate request, TaskCreationOptions.None, ct)
    
    member private self.AsyncDelete(request: DeleteRequest): Async<unit> =
        async {
            if request.Type = domainRecordResourceName then
                match internalHttpClient with
                | None ->
                    // Provider has no token configured (legacy provider without apiToken arg).
                    // Skip the HTTP call since we can't authenticate.
                    ()
                | Some _ ->
                    let _, domainName = request.Properties.["domainName"].TryGetString()
                    do! self.AsyncSetDefaultNameservers domainName

                    let propertiesWithEmptyRecords =
                        readOnlyDict
                            [
                                "domainName", PropertyValue domainName
                                "records", PropertyValue ImmutableArray.Empty
                            ]
                        |> ImmutableDictionary.CreateRange
                        
                    do! self.AsyncCreateOrUpdate propertiesWithEmptyRecords request.Properties
            else
                failwith $"Unknown resource type '{request.Type}'"
        }

    override self.Delete (request: DeleteRequest, ct: CancellationToken): Task = 
        Async.StartAsTask(self.AsyncDelete request, TaskCreationOptions.None, ct)

    member private self.AsyncRead (request: ReadRequest) : Async<ReadResponse> =
        async {
            if request.Type = domainRecordResourceName then
                let _, domainName = request.Properties.["domainName"].TryGetString()
                let! getRecordsResult = self.AsyncGetDnsRecords domainName
                
                match getRecordsResult with
                | None ->
                    let! nameservers = self.AsyncGetNameservers domainName
                    let properties = 
                        dict [
                            "domainName", PropertyValue domainName
                            "nameservers", nameservers |> Seq.map PropertyValue |> ImmutableArray.CreateRange |> PropertyValue
                        ]
                    return ReadResponse(Id = request.Id, Properties = properties)
                | Some records ->
                    let recordPropertyValues =
                        records 
                        |> Seq.map (fun record -> record.ToImmutableDictionary() |> PropertyValue)
                    let properties = 
                        dict [
                            "domainName", PropertyValue domainName
                            "records", recordPropertyValues |> ImmutableArray.CreateRange |> PropertyValue
                        ]
                    return ReadResponse(Id = request.Id, Properties = properties)
            else
                return failwith $"Unknown resource type '{request.Type}'"
        }

    override self.Read (request: ReadRequest, ct: CancellationToken): Task<ReadResponse> = 
        Async.StartAsTask(self.AsyncRead request, TaskCreationOptions.None, ct)
