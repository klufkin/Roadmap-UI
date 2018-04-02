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
      , posts =
            [ { id = 0, description = "first" }
            , { id = 1, description = "hello there" }
            , { id = 2, description = "wowzers" }
            ]
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


getCurrentPosition : Mouse.Position -> PostDrag -> PostDrag
getCurrentPosition xy postDrag =
    { postDrag | current = xy }


shiftPosts : Int -> Int -> List Post -> List Post
shiftPosts newIndex selectedIndex posts =
    let
        beforeSelected selectedIndex =
            List.take selectedIndex posts

        betweenShiftDown =
            posts
                |> List.drop (selectedIndex + 1)
                |> List.take (newIndex - selectedIndex)

        betweenShiftUp =
            posts
                |> List.take selectedIndex
                |> List.drop newIndex

        afterDrop dropIndex =
            List.drop (dropIndex + 1) posts

        shiftDown selectedPost selectedIndex dropIndex =
            List.concat
                [ beforeSelected selectedIndex
                , betweenShiftDown
                , [ selectedPost ]
                , afterDrop dropIndex
                ]

        shiftUp selectedPost selectedIndex dropIndex =
            List.concat
                [ beforeSelected dropIndex
                , [ selectedPost ]
                , betweenShiftUp
                , afterDrop selectedIndex
                ]

        shift selectedIndex newIndex post =
            if selectedIndex < newIndex then
                shiftDown post selectedIndex newIndex
            else
                shiftUp post selectedIndex newIndex
    in
        case
            List.drop selectedIndex posts
                |> List.head
        of
            Just post ->
                shift selectedIndex newIndex post

            Nothing ->
                posts


screenTopOffset : Int
screenTopOffset =
    104


postHeight : Int
postHeight =
    64


dropPost : PostDrag -> Mouse.Position -> Model -> ( Model, Cmd msg )
dropPost { start, postIndex } end model =
    let
        dy =
            end.y - start.y

        offSetFromPrevPost =
            (start.y - screenTopOffset) % postHeight

        distToNextPost =
            postHeight - offSetFromPrevPost

        movingDown =
            dy > 0

        movingUp =
            dy < 0

        crossedToPrev =
            abs dy > offSetFromPrevPost

        crossedToNext =
            abs dy > distToNextPost

        newIndex =
            (end.y - screenTopOffset) // postHeight

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
            [ viewPostsWhileDragging model

            -- pass Maybe PostDrag value to 'andThen'
            , model.postDrag
                -- Finds the selected Maybe Post and passes on
                |> Maybe.andThen (\{ postIndex } -> getPostByIndex postIndex model.posts)
                -- transform into Maybe Html msg
                |> Maybe.map (viewDraggingPost model.postDrag)
                -- if Nothing set Default to empty string
                |> Maybe.withDefault (text "")
            ]
        ]


viewDraggingPost : Maybe PostDrag -> Post -> Html msg
viewDraggingPost postDrag post =
    case postDrag of
        Just { current } ->
            let
                textAreaHeight =
                    40

                postWidth =
                    320
            in
                div
                    [ class "post-container dragging-post"
                    , style
                        [ ( "top", px (current.y - (textAreaHeight // 2)) )
                        , ( "left", px (current.x - (postWidth // 2)) )
                        ]
                    ]
                    [ textarea [ value post.description ] [] ]

        Nothing ->
            text ""


viewPostsWhileDragging : Model -> Html Msg
viewPostsWhileDragging model =
    let
        posts =
            indexedMap postView model.posts

        dragIndex =
            case model.postDrag of
                Just post ->
                    post.postIndex

                Nothing ->
                    -1

        nonDragPosts =
            List.concat [ List.take dragIndex posts, List.drop (dragIndex + 1) posts ]

        viewPostsWithDropZone { y } =
            let
                dropIndex =
                    (y - screenTopOffset) // postHeight
            in
                List.concat
                    [ List.take dropIndex nonDragPosts
                    , [ viewDropZone ]
                    , List.drop dropIndex nonDragPosts
                    ]
    in
        div []
            (model.postDrag
                |> Maybe.map (\{ current } -> viewPostsWithDropZone current)
                |> Maybe.withDefault posts
            )


viewPosts : List Post -> Html Msg
viewPosts posts =
    div [] (indexedMap postView posts)


viewDropZone : Html Msg
viewDropZone =
    div [ class "dropzone-container" ]
        [ div [ class "dropzone" ] []
        ]


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


px : Int -> String
px int =
    toString int ++ "px"
