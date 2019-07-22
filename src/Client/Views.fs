module Views
    open Models
    open Fulma
    open Fable.React

    let button txt onClick =
        Button.button
            [
              Button.Color IsPrimary
              Button.OnClick onClick ]
            [ str txt ]

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

    let myAboutView (model : Model) (dispatch : Msg -> unit) =
        let content =  [ Content.content [ Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
                            [ Heading.h3 [] [ str ( "About Elmish" ) ] ]
                       ]
        content |> main model dispatch