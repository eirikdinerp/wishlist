module Wishlists exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random exposing (..)
import String


-- MODEL





type alias Model =
    { draftWish : Wish
    , wishes : List Wish
    , draftList : WishList 
    , wishLists : List WishList
    }

type alias WishList = 
    { name : String 
    , wishes : List Wish 
    , listId : Int 
    }

type alias Wish =
    { description : String
    , url : String
    , uid : Int
    , listId: Int
    }




init : ( Model, Cmd Msg )
init =
    ( emptyModel, Cmd.none )


emptyModel : Model
emptyModel =
    { draftWish = draftWish
    , wishes = []
    , draftList = draftList
    , wishLists = []
    }

draftList : WishList 
draftList = 
    { name = "" 
    , wishes = [] 
    , listId = -1 
    }

newList : String -> List Wish -> Int-> WishList
newList name_ wishes_ listId_ =
    { name = name_
    , wishes = wishes_
    , listId = listId_
    }


draftWish : Wish
draftWish =
    { description = ""
    , url = ""
    , uid = -1
    ,listId = -1
    }




newWish : Int -> String -> String -> Int-> Wish
newWish uid_ desc_ url_ listId_ =
    { description = desc_
    , url = url_
    , uid = uid_
    , listId = listId_
    }


isWishInvalid : Wish -> Bool
isWishInvalid wish =
    String.isEmpty wish.description

isNameInvalid : WishList -> Bool 
isNameInvalid wishList = 
    String.isEmpty wishList.name


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
    | ClearInput 
    | RemoveAll
    | CreateList Int
    | AddList
    | UpdateDraftName String
    | UpdateList Int WishList
    | UpdateListDescription Int String
    | DeleteList Int






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
                            model.wishes ++ [ newWish uid wish.description wish.url 0]
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

        ClearInput ->
            let
                currentDraft =
                    model.draftWish

                newDraft =
                    { currentDraft | description = "" }
            in
                ( { model
                    | draftWish = newDraft
                  }
                , Cmd.none
                )
        RemoveAll ->
            ({ model | wishes = []}, Cmd.none)
        CreateList uid ->
            let
                wishList =
                    model.draftList
            in
                ( { model
                    | wishLists =
                        if isNameInvalid wishList then
                            model.wishLists
                        else
                            model.wishLists ++ [ newList wishList.name wishList.wishes uid]
                    , draftList = draftList
                  }
                , Cmd.none
                )
        AddList ->
            ( model, Random.generate CreateList (Random.int 1 100) )
        UpdateDraftName name ->
            let
                currentDraft =
                    model.draftList

                newDraft =
                    { currentDraft | name = name }
            in
                ( { model
                    | draftList = newDraft
                  }
                , Cmd.none
                )
        UpdateList uid wishList ->
            let
                updateList w =
                    if w.listId == uid then
                        { w | name = wishList.name }
                    else
                        w
            in
                ( { model | wishLists = List.map updateList model.wishLists }, Cmd.none )
        UpdateListDescription uid desc ->
            let
                updateList w =
                    if w.listId == uid then
                        { w | name = desc }
                    else
                        w
            in
                ( { model | wishLists = List.map updateList model.wishLists }, Cmd.none )
        DeleteList listId ->
            ( { model | wishLists = List.filter (\t -> t.listId /= listId) model.wishLists }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW



view : Model -> Html.Html Msg 
view model =
    div [ style [ ( "padding", "20px" ) ] ]
        [ viewWishLists model
        , viewWishes model
        ]

viewWishLists : Model -> Html Msg
viewWishLists model = 
    div 
        [ style [] ]
        [ h1 [] 
            [ text "Wishlists"]
        , p [] 
            [ text "Add Wishlist"
            ]
        , input 
            [ placeholder "name"
            , onInput UpdateDraftName 
            , value model.draftList.name
            ] []
        , button 
            [ onClick AddList 
            ]
            [ text "Add" ]
        , p []
            [ text "Your wishlists"
            ]
        , ul
            [ style
                [ ( "list-style-type", "none" )
                , ( "padding", "0" )
                , ( "margin", "0" )
                ]
            ]
            (List.map viewList model.wishLists)
        , div [ style [ ( "margin-top", "60px" ) ] ]
            [ text (toString model.wishLists)
            , text (toString model.draftList)
            ]
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
            , value model.draftWish.description
            -- , value model.draft.description
            ]
            []
        , input
            [ placeholder "URL"
            , onInput UpdateDraftUrl
            , value model.draftWish.url
            ]
            []
        , button 
            [ onClick AddWish 
            ]
            [ text "Add Wish" ]
        , button 
            [ onClick RemoveAll 
            ]
            [ text "Remove All" ]
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

viewList : WishList -> Html Msg
viewList wishList = 
    li []
        [ input
            [ placeholder "Name"
            , value wishList.name
            , onInput (UpdateListDescription wishList.listId)
            ]
            []
        , text ("ID: " ++ toString wishList.listId)
        , button [onClick (DeleteList wishList.listId) ]
            [ text "Delete " ]
        , button [][ text "Select" ]
        ]



main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
