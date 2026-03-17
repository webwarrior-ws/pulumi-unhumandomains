open System
open System.Threading

open Pulumi.Experimental.Provider

open Pulumi.UnhumanDomains

[<EntryPoint>]
let main args =
    let apiToken = 
        // Allow empty token for cases when provider is used to get schema for SDK.
        // Do a check in SherlockDomainsProvider.Configure method instead.
        Environment.GetEnvironmentVariable UnhumanDomainsProvider.ManagementTokenEnvVarName
    
    Provider.Serve(args, UnhumanDomainsProvider.Version, (fun _host -> new UnhumanDomainsProvider(apiToken)), CancellationToken.None)
    |> Async.AwaitTask
    |> Async.RunSynchronously
    0
