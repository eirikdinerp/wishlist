module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random exposing (..)
import String


-- MODEL


testDataUsers : List User
testDataUsers =
    [ { uid = 1, username = "oracle", email = "oracle@oracle.com" }
    , { uid = 2, username = "varjen", email = "varjen@varjen.com" }
    , { uid = 3, username = "budull", email = "budull@budull.com" }
    ]


testDataWishes : List Wish
testDataWishes =
    [ { uid = 1, isBought = False, description = "Bicycle", url = "http://greatbikes.com", userId = 1 }
    , { uid = 2, isBought = False, description = "Sweater", url = "http://greatsweaters.com", userId = 1 }
    , { uid = 3, isBought = False, description = "Giftcard @ amazon", url = "http://amazon.com", userId = 1 }
    , { uid = 4, isBought = False, description = "Lamp", url = "http://greatlamps.com", userId = 1 }
    , { uid = 5, isBought = False, description = "China", url = "http://greatchina.com", userId = 2 }
    ]


type alias Model =
    { draftUser : User
    , users : List User
    , draftWish : Wish
    , wishes : List Wish
    }


type alias Wish =
    { uid : Int
    , isBought : Bool
    , description : String
    , url : String
    , userId : Int
    }


type alias User =
    { uid : Int
    , username : String
    , email : String
    }


init : ( Model, Cmd Msg )
init =
    ( emptyModel, Cmd.none )


emptyModel : Model
emptyModel =
    { draftWish = draftWish
    , wishes = testDataWishes
    , users = testDataUsers
    , draftUser = draftUser
    }


draftWish : Wish
draftWish =
    { description = ""
    , isBought = False
    , url = ""
    , uid = -1
    , userId = -1
    }


draftUser : User
draftUser =
    { uid = -1
    , username = ""
    , email = ""
    }


newUser : Int -> String -> String -> User
newUser uid_ username_ email_ =
    { username = username_
    , email = email_
    , uid = uid_
    }


isUserValid : User -> Bool
isUserValid user =
    String.isEmpty user.username || String.isEmpty user.email


newWish : Int -> String -> String -> Wish
newWish uid_ desc_ url_ =
    { description = desc_
    , isBought = False
    , url = url_
    , uid = uid_
    , userId = 1
    }


isWishInvalid : Wish -> Bool
isWishInvalid wish =
    String.isEmpty wish.description || String.isEmpty wish.url



-- UPDATE


type Msg
    = NoOp
    | AddWish
    | CreateWish Int
    | UpdateDraftDesc String
    | UpdateDraftUrl String
    | UpdateWish Int Wish
    | UpdateWishDescription Int String
    | UpdateWishUrl Int String
    | DeleteWish Int
    | AddUser
    | CreateUser Int
    | UpdateDraftUserName String
    | UpdateDraftUserEmail String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        CreateWish uid ->
            let
                wish =
                    model.draftWish
            in
                ( { model
                    | wishes =
                        if isWishInvalid wish then
                            model.wishes
                        else
                            model.wishes ++ [ newWish uid wish.description wish.url ]
                    , draftWish = draftWish
                  }
                , Cmd.none
                )

        UpdateDraftDesc desc ->
            let
                currentDraft =
                    model.draftWish

                newDraft =
                    { currentDraft | description = desc }
            in
                ( { model
                    | draftWish = newDraft
                  }
                , Cmd.none
                )

        UpdateDraftUrl url ->
            let
                currentDraft =
                    model.draftWish

                newDraft =
                    { currentDraft | url = url }
            in
                { model
                    | draftWish = newDraft
                }
                    ! []

        UpdateWish uid wish ->
            let
                updateWish w =
                    if w.uid == uid then
                        { w | description = wish.description, url = wish.url }
                    else
                        w
            in
                ( { model | wishes = List.map updateWish model.wishes }, Cmd.none )

        UpdateWishDescription uid desc ->
            let
                updateWish w =
                    if w.uid == uid then
                        { w | description = desc }
                    else
                        w
            in
                ( { model | wishes = List.map updateWish model.wishes }, Cmd.none )

        UpdateWishUrl uid url ->
            let
                updateWish w =
                    if w.uid == uid then
                        { w | url = url }
                    else
                        w
            in
                ( { model | wishes = List.map updateWish model.wishes }, Cmd.none )

        DeleteWish uid ->
            ( { model | wishes = List.filter (\t -> t.uid /= uid) model.wishes }, Cmd.none )

        AddWish ->
            ( model, Random.generate CreateWish (Random.int 1 100) )

        CreateUser uid ->
            let
                currentDraftUser =
                    model.draftUser
            in
                ( { model
                    | users =
                        if isUserValid currentDraftUser then
                            model.users
                        else
                            model.users ++ [ newUser uid currentDraftUser.username currentDraftUser.email ]
                    , draftUser = draftUser
                  }
                , Cmd.none
                )

        AddUser ->
            ( model, Random.generate CreateUser (Random.int 200 300) )

        UpdateDraftUserName username ->
            let
                draftUser =
                    model.draftUser

                updatedUser =
                    { draftUser | username = username }
            in
                ( { model | draftUser = updatedUser }, Cmd.none )

        UpdateDraftUserEmail email ->
            let
                draftUser =
                    model.draftUser

                updatedUser =
                    { draftUser | email = email }
            in
                ( { model | draftUser = updatedUser }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [ style [ ( "padding", "20px" ) ] ]
        [ viewWishes model
        , viewUsers model
        ]


viewWishes : Model -> Html Msg
viewWishes model =
    div
        [ style [] ]
        [ h1 []
            [ text "Wishlist Management" ]
        , p []
            [ text "Create a new wish"
            ]
        , input
            [ placeholder "Description"
            , onInput UpdateDraftDesc

            -- , value model.draft.description
            ]
            []
        , input
            [ placeholder "URL"
            , onInput UpdateDraftUrl
            ]
            []
        , button [ onClick AddWish ]
            [ text "Add Wish" ]
        , p []
            [ text "Your wishes"
            ]
        , ul
            [ style
                [ ( "list-style-type", "none" )
                , ( "padding", "0" )
                , ( "margin", "0" )
                ]
            ]
            (List.map viewWish model.wishes)
        , div [ style [ ( "margin-top", "60px" ) ] ]
            [ text (toString model.wishes)
            , text (toString model.draftWish)
            ]
        ]


viewWish : Wish -> Html Msg
viewWish wish =
    li []
        [ input
            [ placeholder "Description"
            , value wish.description
            , onInput (UpdateWishDescription wish.uid)
            ]
            []
        , input
            [ placeholder "URL"
            , value wish.url
            ]
            []
        , text ("ID: " ++ toString wish.uid)
        , button [ onClick (DeleteWish wish.uid) ]
            [ text "Delete " ]
        ]


viewUsers : Model -> Html Msg
viewUsers model =
    div
        [ style [] ]
        [ h1 []
            [ text "User Management" ]
        , p []
            [ text "Create a new user"
            ]
        , input
            [ placeholder "Username"
            , onInput UpdateDraftUserName
            ]
            []
        , input
            [ placeholder "email"
            , onInput UpdateDraftUserEmail
            ]
            []
        , button [ onClick AddUser ]
            [ text "Add user" ]
        , p []
            [ text "All users"
            ]
        , ul
            [ style
                [ ( "list-style-type", "none" )
                , ( "padding", "0" )
                , ( "margin", "0" )
                ]
            ]
            (List.map viewUser model.users)
        , div [ style [ ( "margin-top", "60px" ) ] ]
            [ text (toString model.users) ]
        ]


viewUser : User -> Html Msg
viewUser user =
    li []
        [ input
            [ placeholder "Username"
            , value user.username
            ]
            []
        , input
            [ placeholder "Email"
            , value user.email
            ]
            []
        , text ("UserId: " ++ toString user.uid)
        , button [ onClick NoOp ]
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
