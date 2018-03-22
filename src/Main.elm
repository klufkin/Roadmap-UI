module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick, onBlur, on)
import Json.Decode as Json
import Dom exposing (..)
import Task
import List exposing (map, filter, indexedMap, take, drop, concat)
import Mouse


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Post =
    { id : Int, description : String }


type alias PostDrag =
    { start : Mouse.Position
    , current : Mouse.Position
    , postIndex : Int
    }


type alias Model =
    { uid : Int
    , posts : List Post
    , postDrag : Maybe PostDrag
    }


init : ( Model, Cmd Msg )
init =
    ( { uid = 0
      , posts = [ { id = 0, description = "first" } ]
      , postDrag = Nothing
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = NoOp
    | Add Int
    | Update Int String
    | Delete Int
    | PostDragStart Int Mouse.Position
    | PostDragging PostDrag Mouse.Position
    | PostDragEnd PostDrag Mouse.Position


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Add index ->
            let
                start =
                    take index model.posts

                length =
                    List.length model.posts

                end =
                    drop index model.posts

                newID =
                    model.uid + 1

                updatedPosts =
                    concat [ start, [ { id = newID, description = "" } ], end ]

                focus =
                    Dom.focus ("post-" ++ toString newID)
            in
                ( { model | uid = newID, posts = updatedPosts }
                , Task.attempt (\_ -> NoOp) focus
                )

        Update id texts ->
            let
                updatePost post =
                    if post.id == id then
                        { post | description = texts }
                    else
                        post
            in
                ( { model | posts = List.map updatePost model.posts }, Cmd.none )

        Delete id ->
            let
                removePost post =
                    post.id /= id
            in
                ( { model | posts = filter removePost model.posts }, Cmd.none )

        PostDragStart index xy ->
            ( { model | postDrag = Just <| PostDrag xy xy index }, Cmd.none )

        PostDragging postDrag xy ->
            ( { model | postDrag = Just <| getCurrentPosition xy postDrag }, Cmd.none )

        PostDragEnd postDrag xy ->
            model
                |> dropPost postDrag xy



-- ( { model | postDrag = Nothing }, Cmd.none )


getCurrentPosition : Mouse.Position -> PostDrag -> PostDrag
getCurrentPosition xy postDrag =
    { postDrag | current = xy }


shiftPosts : Int -> Int -> List Post -> List Post
shiftPosts newIndex selectedIndex posts =
    let
        beforeSelected selectedIndex =
            List.take selectedIndex posts

        betweenSelectedAndDrop selectedIndex dropIndex =
            posts
                |> List.drop (selectedIndex + 1)
                |> List.take (dropIndex - selectedIndex)

        -- selectedPost =
        --     List.drop selectedIndex posts
        --         |> List.head
        afterDrop dropIndex =
            List.drop (dropIndex + 1) posts

        shiftDown selectedPost selectedIndex dropIndex =
            List.concat
                [ beforeSelected selectedIndex
                , betweenSelectedAndDrop selectedIndex dropIndex
                , [ selectedPost ]
                , Debug.log "AfterDrop" (afterDrop dropIndex)
                ]
    in
        case
            List.drop selectedIndex posts
                |> List.head
        of
            Just post ->
                shiftDown post selectedIndex newIndex

            Nothing ->
                posts


dropPost : PostDrag -> Mouse.Position -> Model -> ( Model, Cmd msg )
dropPost { start, postIndex } end model =
    let
        dy =
            end.y - start.y

        postHeight =
            62

        offSetFromPrevPost =
            start.y % postHeight

        distToNextPost =
            postHeight - offSetFromPrevPost

        movingDown =
            dy > 0

        movingUp =
            dy < 0

        crossedToPrev =
            dy > offSetFromPrevPost

        crossedToNext =
            abs dy > distToNextPost

        screenTopOffset =
            80

        newIndex =
            Debug.log "newIndex" ((end.y - screenTopOffset) // postHeight)

        newPosts =
            if (movingUp && crossedToPrev) || (movingDown && crossedToNext) then
                shiftPosts newIndex postIndex model.posts
            else
                model.posts
    in
        ( { model | posts = newPosts, postDrag = Nothing }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.postDrag of
        Nothing ->
            Sub.none

        Just postDrag ->
            Sub.batch
                [ Mouse.moves (PostDragging postDrag)
                , Mouse.ups (PostDragEnd postDrag)
                ]



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Road Map" ]
        , addBtn 0
        , div []
            [ viewPosts model.posts

            -- pass Maybe PostDrag value to 'andThen'
            , model.postDrag
                -- Finds the selected Maybe Post and passes on
                |> Maybe.andThen (\{ postIndex } -> getPostByIndex postIndex model.posts)
                -- transform int0 Maybe Html msg
                |> Maybe.map (viewDraggingPost model.postDrag)
                -- if Nothing set Default to empty string
                |> Maybe.withDefault (text "")
            ]
        ]


viewDraggingPost : Maybe PostDrag -> Post -> Html msg
viewDraggingPost postDrag post =
    case postDrag of
        Just { current } ->
            div
                [ class "post-container dragging-post"
                , style [ ( "top", px current.y ), ( "left", px current.x ) ]
                ]
                [ textarea [ value post.description ] [] ]

        Nothing ->
            text ""


viewPosts : List Post -> Html Msg
viewPosts posts =
    div [] (indexedMap postView posts)


addBtn : Int -> Html Msg
addBtn index =
    div [ class "add-btn" ]
        [ button [ onClick (Add index) ]
            [ text "+" ]
        ]


postView : Int -> Post -> Html Msg
postView index post =
    div
        []
        [ div
            [ class "post-container"
            , on "mousedown" <| Json.map (PostDragStart index) Mouse.position
            ]
            [ textarea
                [ autofocus True
                , id ("post-" ++ toString post.id)
                , onInput (Update post.id)
                , onBlur
                    (if String.length post.description == 0 then
                        Delete post.id
                     else
                        NoOp
                    )
                , value post.description
                ]
                []
            ]
        , addBtn (index + 1)
        ]



-- HELPERS


getPostByIndex : Int -> List Post -> Maybe Post
getPostByIndex index posts =
    List.drop index posts
        |> List.head


px int =
    toString int ++ "px"
