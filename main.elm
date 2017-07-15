-- Read all about this program in the official Elm guide:
-- https://guide.elm-lang.org/architecture/user_input/forms.html

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Dict

main =
  Html.beginnerProgram
    { model = model
    , view = view
    , update = update
    }



-- MODEL


type alias Model =
  {
    size : Int,
    breakDown : List DB
  }


model : Model
model =
  Model 0 []



-- UPDATE

type alias Shit = {
  remaining : Float,
  coins : List DB
 }

fun: Float -> List DB -> List DB
fun sum allCoins =
  let
    fn coin m = { remaining = m.remaining - ((toFloat ((if coin.size == 0.5 then ceiling else floor) (m.remaining / coin.size))) * coin.size), coins = (List.append m.coins (List.repeat  ((if coin.size == 0.5 then ceiling else floor) (m.remaining / coin.size)) coin)) }
  in
   ( List.foldr fn {remaining = sum , coins = []}  allCoins).coins

type alias DB = {
  name: String,
  size: Float,
  price: Float
}
dbs : List DB
dbs = [
  DB "cache.t2.micro" 0.5 0.017,
  DB "cache.t2.small" 1.55 0.034,
  DB "cache.m3.medium" 2.78 0.090,
  DB "cache.t2.medium" 3.22 0.068,

  DB "cache.m3.large" 6.05 0.182,
  DB "cache.m4.large" 6.42 0.156,
  DB "cache.r3.large" 13.5 0.228,

  DB "cache.m3.xlarge" 13.3 0.364,
  DB "cache.m4.xlarge" 14.28 0.311,
  DB "cache.r3.xlarge" 28.4 0.455,

  DB "cache.m3.2xlarge" 27.9 0.728,
  DB "cache.m4.2xlarge" 29.70 0.623,
  DB "cache.r3.2xlarge" 58.2 0.910,

  DB "cache.m3.2xlarge" 27.9 0.728,
  DB "cache.m4.2xlarge" 29.70 0.623,
  DB "cache.r3.2xlarge" 58.2 0.910,

  DB "cache.m4.4xlarge" 60.78 1.245,
  DB "cache.r3.4xlarge" 118 1.820,

  DB "cache.m4.10xlarge" 154.64 3.112,
  DB "cache.r3.8xlarge" 237 3.640
 ]


type Msg
    = Size String

update : Msg -> Model -> Model
update msg model =
  case msg of
    Size size ->
      let
        newInt = Result.withDefault 0 (String.toInt size)
      in

      { model | size =  newInt , breakDown = fun (toFloat newInt) dbs }


-- VIEW

sumDbs db count = db.size + count
costDbs db cost = db.price + cost
mergeResults: DB -> Dict.Dict String Int  -> Dict.Dict  String Int
mergeResults db result =
  let
    count = Maybe.withDefault 0 (Dict.get (toString db) result)
  in
    (Dict.insert (toString db) (count + 1) result)

emptyDict : Dict.Dict String Int
emptyDict = (Dict.empty)
view : Model -> Html Msg
view model =
  div []
    [
      h1 [][text "A Crummy Elm Based AWS ElasticCache Calculator"],
      input [ type_ "number", placeholder "Total Desired Cluster Size (GB)", onInput Size,  Html.Attributes.min "0" ] [],
      h2 [][text "Suggested Cluster Sizes"],
      div [] (List.map break (Dict.toList (List.foldl mergeResults emptyDict model.breakDown))),
      div [] [text (
        "TOTAL: " ++
        (toString (List.foldl sumDbs 0 model.breakDown))
        ++ "GB" ++ " $" ++
        (toString (round (30 * 24 * (List.foldl costDbs 0 model.breakDown))))
        ++ " (USD Per Month) "
        )]
    ]

break : (String, Int) -> Html msg
break db =
  div [] [ text (toString db)  ]
