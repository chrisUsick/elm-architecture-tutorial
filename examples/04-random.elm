import Html exposing (..)
import Html.Events exposing (..)
import Random
import Input.Number


main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL


type alias Model =
  { diceFaces : List Int
  }


init : (Model, Cmd Msg)
init =
  (Model (List.repeat 2 1), Cmd.none)



-- UPDATE


type Msg
  = Roll
  | NumDice Int
  | NewFaces (List Int)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Roll ->
      (model, Random.generate NewFaces (randomNums 1 6 (List.length model.diceFaces)))

    NumDice num ->
      ({model | diceFaces = (setDiceFaces model.diceFaces num)}, Cmd.none)

    NewFaces newFaces ->
      (Model newFaces, Cmd.none)

setDiceFaces : List Int -> Int -> List Int
setDiceFaces faces num =
  if (num > List.length faces) then
    List.append faces (List.repeat (num - List.length faces) 0)
  else if (num < List.length faces) then
    List.take num faces
  else
    faces

randomNums : Int -> Int -> Int -> Random.Generator (List Int)
randomNums min max size =
  Random.list size (Random.int min max)
-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ h1 [] (drawDice model.diceFaces)
    , button [ onClick Roll ] [ text "Roll" ]
    -- , input [onInput NumDice, type_ "number"] []
    , Input.Number.input
        {onInput = maybeNumDice
        , minValue = Just 0
        , maxValue = Nothing
        , hasFocus = Nothing
        , maxLength = Nothing
        }
        []
        (Just (List.length model.diceFaces))
    ]
maybeNumDice : Maybe Int -> Msg
maybeNumDice x =
  case x of
    Just num ->
      NumDice num
    Nothing ->
      NumDice 0

drawDice : List int -> List (Html Msg)
drawDice faces =
  List.map (\face -> div [] [text (toString face)]) faces
