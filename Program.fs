open System
open System.Threading

open Pulumi.Experimental.Provider

open Pulumi.UnhumanDomains

[<EntryPoint>]
let main args =
    Provider.Serve(args, UnhumanDomainsProvider.Version, (fun host -> new UnhumanDomainsProvider(host)), CancellationToken.None)
    |> Async.AwaitTask
    |> Async.RunSynchronously
    0
