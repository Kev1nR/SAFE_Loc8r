module Client.Models
    open Shared
    open Elmish.UrlParser

    // The model holds data that you want to keep track of while the application is running
    // in this case, we are keeping track of a counter
    // we mark it as optional, because initially it will not be available from the client
    // the initial value will be requested from server
    type ViewModel = { Content: Content option  }

    [<RequireQualifiedAccess>]
    type Page =
    | Home
    | About
    | Location
    | AddReview

    // The Msg type defines what events/actions can occur while the application is running
    // the state of the application changes *only* in reaction to these events
    type Msg =
    | Toggle
    | InitialContentLoaded of Content
    | PageNavigation of Page

    type PageModel =
    | Home of ViewModel
    | About of ViewModel

    let pageParser : Parser<Page -> Page,_> =
        oneOf
            [
              map Page.Home (s "home")
              map Page.About (s "about")
              map Page.Location (s "location")
              map Page.AddReview (s "location/review/new")
            ]

    let urlParser location =
        let path = parseHash pageParser location
        printf "urlParser: locn = %A, path = %A" location path
        path
