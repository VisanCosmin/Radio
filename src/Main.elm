module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder)
import Json.Decode as D


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , view = view
    }

type alias AppState = { stations : (List Station) , currentStation : Maybe Station }
type Model = Init | Loaded AppState

init : () -> (Model, Cmd Msg)
init _ =
  ( Init
  , Http.get
    { url = "https://visancosmin.github.io/Radio/stations.json"
    , expect = Http.expectJson RequestStationsResult listDecoder
    }
  )


type Msg = RequestStations
         | RequestStationsResult (Result Http.Error (List Station))


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of 
    RequestStations -> (Init , Cmd.none)
    (RequestStationsResult res) -> 
      case res of (Err _) -> (Loaded {stations = [] , currentStation = Nothing} , Cmd.none)
                  (Ok s) -> (Loaded {stations = s , currentStation = Nothing} , Cmd.none)

view : Model -> Html Msg
view model =
  case model of 
    Init -> div [] [ text "Hello World" ]
    (Loaded data) -> viewApp data


viewApp : AppState -> Html Msg
viewApp state = 
  div [] 
      [ div [ class "navbar" ] [ h3 [] [ text "RADIO WORLD" ] ] 
      , node "link" [ href "https://fonts.googleapis.com/css2?family=Open+Sans:wght@300;400;600;700;800&display=swap" 
             , rel "stylesheet" ] []
      , node "link" [ href "https://visancosmin.github.io/Radio/main2.css" 
             , rel "stylesheet" ] [] 

      ]


type alias Country = { name : String , thumbnail : String }
type alias Station = 
  { name : String 
  , stream : String 
  , thumbnail : String 
  , country : Country
  , categories : List String
  }

stationDecoder : Decoder Station 
stationDecoder = D.map5 Station
  (D.field "name" D.string)
  (D.field "stream" D.string)
  (D.field "thumbnail" D.string)
  (D.field "country" countryDecoder)
  (D.field "categories" (D.list D.string))

listDecoder : Decoder (List Station)
listDecoder = 
  D.field "stations" (D.list stationDecoder)

countryDecoder : Decoder Country
countryDecoder = D.map2 Country
  (D.field "name" D.string)
  (D.field "thumbnail" D.string)