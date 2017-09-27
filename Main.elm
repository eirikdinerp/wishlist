module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random exposing (..)
import String


-- MODEL


type alias Model =
    { draft : Wish
    , wishes : List Wish
    }


type alias Wish =
    { uid : Int
    , isBought : Bool
    , description : String
    , url : String
    }


init : ( Model, Cmd Msg )
init =
    ( emptyModel, Cmd.none )


emptyModel : Model
emptyModel =
    { draft = draftWish
    , wishes = []
    }


draftWish : Wish
draftWish =
    { description = ""
    , isBought = False
    , url = ""
    , uid = -1
    }


newWish : Int -> String -> String -> Wish
newWish uid_ desc_ url_ =
    { description = desc_
    , isBought = False
    , url = url_
    , uid = uid_
    }


isWishInvalid : Wish -> Bool
isWishInvalid wish =
    String.isEmpty wish.description || String.isEmpty wish.url



-- UPDATE


type Msg
    = Add
    | PrepareAdd Int
    | UpdateDraftDesc String
    | UpdateDraftUrl String
    | Delete Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PrepareAdd uid ->
            let
                wish =
                    model.draft
            in
                ( { model
                    | wishes =
                        if isWishInvalid wish then
                            model.wishes
                        else
                            model.wishes ++ [ newWish uid wish.description wish.url ]
                    , draft = draftWish
                  }
                , Cmd.none
                )

        UpdateDraftDesc desc ->
            let
                currentDraft =
                    model.draft

                newDraft =
                    { currentDraft | description = desc }
            in
                ( { model
                    | draft = newDraft
                  }
                , Cmd.none
                )

        UpdateDraftUrl url ->
            let
                currentDraft =
                    model.draft

                newDraft =
                    { currentDraft | url = url }
            in
                { model
                    | draft = newDraft
                }
                    ! []

        Delete uid ->
            ( { model | wishes = List.filter (\t -> t.uid /= uid) model.wishes }, Cmd.none )

        Add ->
            ( model, Random.generate PrepareAdd (Random.int 1 100) )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ style [ ( "padding", "20px" ) ] ]
        [ h1 []
            [ text "Wishlist Application" ]
        , p []
            [ text "Create a new wish"
            ]
        , input
            [ placeholder "Description"
            , onInput UpdateDraftDesc
            , value model.draft.description
            ]
            []
        , input
            [ placeholder "URL"
            , onInput UpdateDraftUrl
            , value model.draft.url
            ]
            []
        , button [ onClick Add ]
            [ text "Add Wish" ]
        , p []
            [ text "Your wishes"
            ]
        , ul []
            (List.map viewWish model.wishes)
        ]


viewWish : Wish -> Html Msg
viewWish wish =
    li []
        [ text (toString wish.uid ++ ", " ++ wish.description ++ ", " ++ wish.url)
        , button [ onClick (Delete wish.uid) ]
            [ text "Delete " ]
        ]


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
