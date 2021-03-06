open System.IO
open System.Threading.Tasks

open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.DependencyInjection
open FSharp.Control.Tasks.V2
open Giraffe
open Saturn
open Shared


let tryGetEnv = System.Environment.GetEnvironmentVariable >> function null | "" -> None | x -> Some x

let publicPath = Path.GetFullPath "../Client/public"

let port =
    "PORT"
    |> tryGetEnv |> Option.map uint16 |> Option.defaultValue 8085us

let getTitle() : Task<Content> = task { return { Title = "Elmish"; ButtonLabel = "Reverse" }}
let webApp = router {
    get "/api/init" (fun next ctx ->
        task {
            let! content = getTitle()
            return! json content next ctx
        })
    get "/api/" (fun next ctx ->
        task {
            let! content = task { return { Title = "Home"; ButtonLabel = "Reverse Home" }}
            return! json content next ctx
        })
    get "/api/about" (fun next ctx ->
        task {
            let! content = task { return { Title = "About"; ButtonLabel = "Reverse About" }}
            return! json content next ctx
        })
}

let app = application {
    url ("http://0.0.0.0:" + port.ToString() + "/")
    use_router webApp
    memory_cache
    use_static publicPath
    use_json_serializer(Thoth.Json.Giraffe.ThothSerializer())
    use_gzip
}

run app
