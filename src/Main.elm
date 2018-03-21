module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick, onBlur)
import Dom exposing (..)
import Task


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


type alias Model =
    { uid : Int
    , posts : List Post
    }


init : ( Model, Cmd Msg )
init =
    ( { uid = 0
      , posts = [ { id = 0, description = "first" } ]
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = NoOp
    | Add Int
    | Update Int String
    | Delete Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Add index ->
            let
                start =
                    List.take index model.posts

                length =
                    List.length model.posts

                end =
                    List.drop index model.posts

                newID =
                    model.uid + 1

                updatedPosts =
                    List.concat [ start, [ { id = newID, description = "" } ], end ]

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
                ( { model | posts = List.filter removePost model.posts }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Road Map" ]
        , addBtn 0
        , div [] (List.map (\( index, post ) -> postView post index) (List.indexedMap (,) model.posts))
        ]


addBtn : Int -> Html Msg
addBtn index =
    div [ class "add-btn" ]
        [ button [ onClick (Add index) ]
            [ text "+" ]
        ]


postView : Post -> Int -> Html Msg
postView post index =
    div []
        [ div [ class "post-container" ]
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
