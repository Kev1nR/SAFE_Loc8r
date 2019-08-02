
module Client.Views
    open Client.Models
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

    let main (model : ViewModel) (dispatch : Msg -> unit) (content : ReactElement list) =
        div [ ]
            [ Heading.h1 [ ] [ str (show model) ]
              p [] [ str ("Welcome to " + (show model)) ]

              Container.container []
                  content
            ]

    let homeView (model : ViewModel) (dispatch : Msg -> unit) =
        let content =  [
                         Content.content [ Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
                            [ button (showLabel model) (fun _ -> dispatch Toggle) ]
                         Content.content [ Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
                            [ a [ Fable.React.Props.Href "/#home"; Props.OnClick (fun _ -> dispatch (PageNavigation Page.Home)) ] [ str "Home" ] ]
                         Content.content [ Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
                            [ a [ Fable.React.Props.Href "/#about"; Props.OnClick (fun _ -> dispatch (PageNavigation Page.About)) ] [ str "About" ] ]
                         Content.content [ Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
                            [ a [ Fable.React.Props.Href "/#location"; Props.OnClick (fun _ -> dispatch (PageNavigation Page.Location)) ] [ str "Location info" ] ]
                         Content.content [ Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
                            [ a [ Fable.React.Props.Href "/#location/review/new"; Props.OnClick (fun _ -> dispatch (PageNavigation Page.AddReview)) ] [ str "Add review" ] ]
                       ]

        content |> main model dispatch

    let aboutView (model : ViewModel) (dispatch : Msg -> unit) =
        let content =  [ Content.content [ Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
                            [ Heading.h3 [] [ str ( "About Elmish" ) ] ]
                       ]
        content |> main model dispatch