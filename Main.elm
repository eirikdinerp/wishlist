
module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)



--MODEL

type alias Wish =
  { description : String 
    , url : String 
  }


type alias Model =
  {   draftWish : Wish 
    , wishlist : List Wish
  }

model : Model
model = 
  { draftWish = draftWish
  , wishlist = []
  }

draftWish : Wish
draftWish =
    { description = ""
    , url = ""
    }

init : ( Model, Cmd Msg )
init =
    ( model, Cmd.none )

-- UPDATE


stylesheet = 
  let 
    tag = 
      "link"

    attrs = 
      [ attribute "Rel" "stylesheet" 
      , attribute "property" "stylesheet" 
      , attribute "href" "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
      ]

    children = 
      []
  in
    node tag attrs children

type Msg 
  = Updatewish String
  | Addwish
  | RemoveAll
  | RemoveItem String
  | ClearInput
  | UpdateUrl String

newWish : Int -> String -> String -> Wish
newWish uid_ desc_ url_ =
    { description = desc_
    , url = url_
    }



update : Msg -> Model ->  Model
update msg model =
  case msg of
    Updatewish text ->
      ({model | wish = text })
    UpdateUrl text ->
      ({ model | url = text}, Cmd.none)
    Addwish ->
      ({ model | wishlist = (model.wish ++ " " ++ model.url) :: model.wishlist}, Cmd.none)
    RemoveAll ->
      ({ model | wishlist = [] }, Cmd.none)
    RemoveItem text ->
      ({ model | wishlist = List.filter (\x -> x/= text) model.wishlist }, Cmd.none)
    ClearInput ->
     ( { model | wish = ""}, Cmd.none)

-- VIEW

wishItem : String -> Html Msg
wishItem wish =
  li [ class "list-group-item" ] [text wish, button [ onClick (RemoveItem wish) ] [ text "x" ] ]

wishList : List String -> Html Msg
wishList wishlist =
  let
    child =
      List.map wishItem wishlist
  in
    ul [] child

view model  = 
  div [ class "jumbotron" ] 
      [ stylesheet
      , input 
        [ type_ "text"
        , placeholder "wish"
        , onInput Updatewish
        , value model.wish
        , class "form-control"
        , onMouseDown ClearInput
        ] []
      , input
        [ type_ "text"
        , placeholder "url"
        , onInput UpdateUrl 
        , value model.url
        , class "form-control"
        , onMouseDown ClearInput 
        ] []
      , button [onClick Addwish, class "btn btn-primary"] [ text "Submit" ]
      , button [onClick RemoveAll, class "btn btn-danger"] [ text "Remove All" ]
      , div [] [wishList model.wishlist ] 

      ]

-- SUBS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = model
        , view = view
        , update = update
        }