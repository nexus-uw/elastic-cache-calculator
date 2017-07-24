-- Read all about this program in the official Elm guide:
-- https://guide.elm-lang.org/architecture/user_input/forms.html

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
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
    breakDown : List DB,
    network : String
  }


model : Model
model =
  Model 0 [] "All"



-- UPDATE

type alias Shit = {
  remaining : Float,
  coins : List DB
 }

fun: Float -> List DB -> List DB
fun sum allDbs =
  let
    minSize =  (Maybe.withDefault (DB "cache.t2.micro" 0.5 0.017 "Low to Moderate") (List.head allDbs) ).size
    fn db m = {
      remaining = m.remaining - ((toFloat ((if db.size == minSize then  ceiling else floor) (m.remaining / db.size))) * db.size),
      result = (List.append m.result (List.repeat  ((if db.size == minSize then ceiling else floor) (m.remaining / db.size)) db))
    }
  in
   ( List.foldr fn {remaining = sum , result = []}  allDbs).result



type alias DB = {
  name: String,
  size: Float,
  price: Float,
  network: String
}

compareByUnitCost: DB -> DB -> Order
compareByUnitCost a b = (compare  (a.size / a.price) (b.size / b.price))

dbs : List DB
dbs = List.sortWith compareByUnitCost [
  DB "cache.t2.micro" 0.5 0.017 "Low to Moderate",
  DB "cache.t2.small" 1.55 0.034 "Low to Moderate",
  DB "cache.m3.medium" 2.78 0.090 "Moderate",
  DB "cache.t2.medium" 3.22 0.068 "Low to Moderate",

  DB "cache.m3.large" 6.05 0.182 "Moderate",
  DB "cache.m4.large" 6.42 0.156 "Moderate",
  DB "cache.r3.large" 13.5 0.228 "Moderate",

  DB "cache.m3.xlarge" 13.3 0.364 "High",
  DB "cache.m4.xlarge" 14.28 0.311 "High",
  DB "cache.r3.xlarge" 28.4 0.455 "Moderate",

  DB "cache.m3.2xlarge" 27.9 0.728 "High",
  DB "cache.m4.2xlarge" 29.70 0.623 "High",
  DB "cache.r3.2xlarge" 58.2 0.910 "High",

  DB "cache.m3.2xlarge" 27.9 0.728 "High",
  DB "cache.m4.2xlarge" 29.70 0.623 "High",
  DB "cache.r3.2xlarge" 58.2 0.910 "High",

  DB "cache.m4.4xlarge" 60.78 1.245 "High",
  DB "cache.r3.4xlarge" 118 1.820 "High",

  DB "cache.m4.10xlarge" 154.64 3.112 "10 Gigabit",
  DB "cache.r3.8xlarge" 237 3.640 "10 Gigabit"
 ]



type Msg
    = Size String
    | LimitTo String

update : Msg -> Model -> Model
update msg model =
  case msg of
   -- set the new size of the cluster
    Size size ->
      let
        newInt = Result.withDefault 0 (String.toInt size)
      in

      {
        model |
        size =  newInt ,
        breakDown = fun (toFloat newInt)  (List.filter (\x -> x.network ==  model.network || model.network == "All") dbs)
       }

    -- set what network level the cluster must be
    LimitTo network ->
      {
        model |
         breakDown = fun (toFloat model.size)  (List.filter (\x -> x.network ==  network || network == "All") dbs)  ,
         network = network
      }

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
      img [class "image_bak", src "ElasticCache.png",  alt "elastic cache logo" , width 120 , height 120][],
      img [class "image", src "anime-eyes.png",  alt "weibo" , width 120 , height 120][],
      div[class "container"] [
        a [href "https://github.com/nexus-uw/elastic-cache-calculator"] [
          img[style [("position", "absolute"), ("top","0"), ("right", "0"), ("border","0")], src "https://camo.githubusercontent.com/a6677b08c955af8400f44c6298f40e7d19cc5b2d/68747470733a2f2f73332e616d617a6f6e6177732e636f6d2f6769746875622f726962626f6e732f666f726b6d655f72696768745f677261795f3664366436642e706e67", alt "Fork me on GitHub"][]
        ],
        h1 [][text "A Crummy Elm Based AWS ElasticCache Calculator"],
        input [ type_ "number", placeholder "Total Desired Cluster Size (GB)", onInput Size,  Html.Attributes.min "0" ] [],
        fieldset [class "fieldgroup"]
          [
            div [] [text "Cluster Network Type"],
            radio "All" (LimitTo "All"),
            radio "Low to Moderate" (LimitTo "Low to Moderate"),
            radio "Moderate" (LimitTo "Moderate"),
            radio "High" (LimitTo "High"),
            radio "10 Gigabit" (LimitTo "10 Gigabit")
          ],
        h2 [][text "Suggested Cluster Size(s)"],
        div [] (List.map break (Dict.toList (List.foldl mergeResults emptyDict model.breakDown))),
        div [] [text (
          "TOTAL: " ++
          (toString (List.foldl sumDbs 0 model.breakDown))
          ++ " GB" )],
      div [] [text (
          " $" ++
          (toString (round (24 * (List.foldl costDbs 0 model.breakDown))))
          ++ " (USD Per Day) "
          )],
        h5 [][text "Disclaimer"],

        div [][text "Prices as of July 16 2017 for US-East-1"],
        div [][text "This is more of a learning exprience for Elm for me than a legit tool. Use the results at your own risk."]
      ]
    ]

-- poor mans display results
break : (String, Int) -> Html msg
break db =
  div [] [ text (toString db)  ]

-- radio button for selecting network size
radio : String -> msg -> Html msg
radio value msg =
  label
    [     ]
    [ input [ type_ "radio", name "network-size", onClick msg ] []
    , text value
    ]
