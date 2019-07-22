module Client

open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props
open Fetch.Types
open Thoth.Fetch
open Fulma
open Thoth.Json

open Shared

// The model holds data that you want to keep track of while the application is running
// in this case, we are keeping track of a counter
// we mark it as optional, because initially it will not be available from the client
// the initial value will be requested from server
type Model = { Content: Content option  }

// The Msg type defines what events/actions can occur while the application is running
// the state of the application changes *only* in reaction to these events
type Msg =
| Toggle
| InitialContentLoaded of Content

let initialContent () = Fetch.fetchAs<Content> "/api/init"

// defines the initial state and initial command (= side-effect) of the application
let init () : Model * Cmd<Msg> =
    let initialModel = { Content = None }
    let loadContentCmd =
        Cmd.OfPromise.perform initialContent () InitialContentLoaded
    initialModel, loadContentCmd

// The update function computes the next state of the application based on the current state and the incoming events/messages
// It can also run side-effects (encoded as commands) like calling the server via Http.
// these commands in turn, can dispatch messages to which the update function will react.
let reverse (str : string) =
    let rec revList revved next =
        match next with
        | [] -> revved
        | h::t -> revList (sprintf "%c%s" h revved) t
    str |> Seq.toList |> revList ""

let update (msg : Msg) (currentModel : Model) : Model * Cmd<Msg> =
    match currentModel.Content, msg with
    | Some content, Toggle when content.ButtonLabel = "Reverse" ->
        let nextModel = { currentModel with Content = Some { Title = reverse (content.Title); ButtonLabel = "Forward" } }
        nextModel, Cmd.none
    | Some content, Toggle when content.ButtonLabel = "Forward" ->
        let nextModel = { currentModel with Content = Some { Title = reverse (content.Title); ButtonLabel = "Reverse" } }
        nextModel, Cmd.none
    | _, InitialContentLoaded initialContent ->
        let nextModel = { Content = Some initialContent }
        nextModel, Cmd.none
    | _ -> currentModel, Cmd.none

let button txt onClick =
    Button.button
        [
          Button.Color IsPrimary
          Button.OnClick onClick ]
        [ str txt ]

module Views =
    let show = function
        | { Content = Some content } -> string content.Title
        | { Content = None   } -> "Loading..."

    let showLabel = function
        | { Content = Some content } -> string content.ButtonLabel
        | { Content = None   } -> "Loading..."

    let main (model : Model) (dispatch : Msg -> unit) (content : ReactElement list) =
        div [ ]
            [ Heading.h1 [ ] [ str (show model) ]
              p [] [ str ("Welcome to " + (show model)) ]

              Container.container []
                  content
            ]

    let homeView (model : Model) (dispatch : Msg -> unit) =
        let content =  [ Content.content [ Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
                            [ button (showLabel model) (fun _ -> dispatch Toggle) ] ]

        content |> main model dispatch

    let aboutView (model : Model) (dispatch : Msg -> unit) =
        let content =  [ Content.content [ Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
                            [ Heading.h3 [] [ str ( "About Elmish" ) ] ]
                       ]
        content |> main model dispatch


// let view (model : Model) (dispatch : Msg -> unit) =
//     div []
//         [ Navbar.navbar [ Navbar.Color IsPrimary ]
//             [ Navbar.Item.div [ ]
//                 [ Heading.h2 [ ]
//                     [ str "SAFE Template" ] ] ]

//           Container.container []
//               [ Content.content [ Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
//                     [ Heading.h3 [] [ str ("Press buttons to manipulate counter: " + show model) ] ]
//                 Columns.columns []
//                     [ Column.column [] [ button "-" (fun _ -> dispatch Decrement) ]
//                       Column.column [] [ button "+" (fun _ -> dispatch Increment) ] ] ]

//           Footer.footer [ ]
//                 [ Content.content [ Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
//                     [ safeComponents ] ] ]

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkProgram init update Views.homeView
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReactBatched "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
