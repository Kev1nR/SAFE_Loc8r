module Client.App

open Elmish
open Elmish.React
open Elmish.UrlParser
open Elmish.Navigation
open Fable.React
open Fable.React.Props
open Fable.Core.JsInterop
open Fable.Import
open Fetch.Types
open Thoth.Fetch
open Fulma
open Thoth.Json
open Shared
open Views
open Client.Models

let initialContent () = Fetch.fetchAs<Content> "/api/init"
let initialHomeContent () = Fetch.fetchAs<Content> "/api/"
let initialAboutContent () = Fetch.fetchAs<Content> "/api/about"

// defines the initial state and initial command (= side-effect) of the application
let init page : ViewModel * Cmd<Msg> =
    match page with
    | Some Page.Home ->
        printf "init selected Page.Home"
        {Content = Some { Title = "Home"; ButtonLabel = "Reverse"} }, Cmd.none
        //Cmd.OfPromise.perform initialHomeContent () InitialContentLoaded
    | Some Page.About ->
        printf "init selected Page.About"
        {Content = Some { Title = "About"; ButtonLabel = "Reverse"} }, Cmd.none
        //Cmd.OfPromise.perform initialAboutContent () InitialContentLoaded
    | None ->
        printf "init selected None"
        {Content = Some { Title = "Home"; ButtonLabel = "Reverse"} }, Cmd.none

// The update function computes the next state of the application based on the current state and the incoming events/messages
// It can also run side-effects (encoded as commands) like calling the server via Http.
// these commands in turn, can dispatch messages to which the update function will react.
let reverse (str : string) =
    let rec revList revved next =
        match next with
        | [] -> revved
        | h::t -> revList (sprintf "%c%s" h revved) t
    str |> Seq.toList |> revList ""

let urlUpdate (result:Page option) (model: ViewModel) =
    printf "urlUpdate %A" result
    match result with
    | None ->
        printf "URL Update with None"
        model, Navigation.modifyUrl ("/#home")
    | Some Page.Home ->
        printf "Url update to Home"
        { model with Content = Some { Title = "Home"; ButtonLabel = "Reverse" } }, Navigation.newUrl("/#home")
    | Some Page.Location ->
        printf "Url update to Location"
        { model with Content = Some { Title = "Location info"; ButtonLabel = "Reverse" } }, Navigation.newUrl("/#location")
    | Some Page.AddReview ->
        printf "Url update to add review"
        { model with Content = Some { Title = "Add review"; ButtonLabel = "Reverse" } }, Navigation.newUrl("/#location/review/new")
    | Some Page.About ->
        printf "Url update to About"
        { model with Content = Some { Title = "About"; ButtonLabel = "Reverse" } }, Navigation.newUrl ("/#about")

let update (msg : Msg) (currentModel : ViewModel) : ViewModel * Cmd<Msg> =
    match currentModel.Content, msg with
    | _, PageNavigation p ->
        printf "Page navigation event to page %A" p
        urlUpdate (Some p) currentModel
    | Some content, Toggle when content.ButtonLabel.StartsWith("Reverse") ->
        printf "Update called"
        let nextModel = { currentModel with Content = Some { Title = reverse (content.Title); ButtonLabel = "Forward" } }
        nextModel, Cmd.none
    | Some content, Toggle when content.ButtonLabel.StartsWith("Forward") ->
        printf "Update called"
        let nextModel = { currentModel with Content = Some { Title = reverse (content.Title); ButtonLabel = "Reverse" } }
        nextModel, Cmd.none
    | _, InitialContentLoaded initialContent ->
        printf "Update called"
        let nextModel = { Content = Some initialContent }
        nextModel, Cmd.none
    | _ ->
        printf "Update called"
        currentModel, Cmd.none

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkProgram init update homeView
|> Program.toNavigable Client.Models.urlParser urlUpdate
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReactBatched "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
