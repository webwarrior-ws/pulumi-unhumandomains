namespace Pulumi.UnhumanDomains

open System
open System.Collections.Immutable
open System.Linq
open System.Net
open System.Net.Http
open System.Threading
open System.Threading.Tasks
open System.Text.Json

open Pulumi
open Pulumi.Experimental
open Pulumi.Experimental.Provider

type UnhumanDomainsProvider(managementToken: string) =
    inherit Pulumi.Experimental.Provider.Provider()

    let httpClient = new HttpClient()

    static let domainRecordResourceName = "unhumandomains:index:Domain"
    static let apiBaseUrl = "https://unhuman.domains"

    do
        httpClient.DefaultRequestHeaders.Authorization <- Headers.AuthenticationHeaderValue("Bearer", managementToken)

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

    static member val ManagementTokenEnvVarName = "UNHUMANDOMAINS_API_TOKEN"

    interface IDisposable with
        override self.Dispose (): unit = 
            httpClient.Dispose()

    member private self.GetDnsRecordPropertyString(dict: ImmutableDictionary<string, PropertyValue>, name: string) =
        match dict.[name].TryGetString() with
        | true, value -> value
        | false, _ -> failwith $"No {name} property in {domainRecordResourceName}"

    member private self.GetDnsRecordPropertyInt(dict: ImmutableDictionary<string, PropertyValue>, name: string) =
        match dict.[name].TryGetNumber() with
        | true, value -> int value
        | false, _ -> failwith $"No {name} property in {domainRecordResourceName}"
    
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
        if String.IsNullOrWhiteSpace managementToken then
            failwithf
                "Environment variable %s not found!"
                UnhumanDomainsProvider.ManagementTokenEnvVarName
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
                    match dnsRecords.TryGetMap() with
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
                        CheckFailure("records", "'records' must be an object") 
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
                return failwith "Not yet implemented"
            else
                return failwith $"Unknown resource type '{request.Type}'"
        }

    override self.Create (request: CreateRequest, ct: CancellationToken): Task<CreateResponse> = 
        Async.StartAsTask(self.AsyncCreate request, TaskCreationOptions.None, ct)

    member private self.AsyncUpdate(request: UpdateRequest): Async<UpdateResponse> =
        async {
            if request.Type = domainRecordResourceName then
                let properties = request.Olds.AddRange request.News
                return failwith "Not yet implemented"
            else
                return failwith $"Unknown resource type '{request.Type}'"
        }

    override self.Update (request: UpdateRequest, ct: CancellationToken): Task<UpdateResponse> = 
        Async.StartAsTask(self.AsyncUpdate request, TaskCreationOptions.None, ct)
    
    member private self.AsyncDelete(request: DeleteRequest): Async<unit> =
        async {
            if request.Type = domainRecordResourceName then
                return failwith "Not yet implemented"
            else
                failwith $"Unknown resource type '{request.Type}'"
        }

    override self.Delete (request: DeleteRequest, ct: CancellationToken): Task = 
        Async.StartAsTask(self.AsyncDelete request, TaskCreationOptions.None, ct)

    member private self.AsyncRead (request: ReadRequest) : Async<ReadResponse> =
        async {
            if request.Type = domainRecordResourceName then
                return failwith "Not yet implemented"
            else
                return failwith $"Unknown resource type '{request.Type}'"
        }

    override self.Read (request: ReadRequest, ct: CancellationToken): Task<ReadResponse> = 
        Async.StartAsTask(self.AsyncRead request, TaskCreationOptions.None, ct)
