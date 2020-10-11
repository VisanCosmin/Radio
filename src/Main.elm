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


type Model = Init | Loaded { stations : (List Station) , currentStation : Maybe Station }

init : () -> (Model, Cmd Msg)
init _ =
  ( Init
  , Http.get
    { url = "https://visancosmin.github.io/Radio/stations.json"
    , expect = Http.expectJson GotResult listDecoder
    }
  )


type Msg = RequestStations
         | GotResult (Result Http.Error (List Station))


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of 
    RequestStations -> (Init , Cmd.none)
    (GotResult res) -> 
      case res of (Err _) -> (Loaded [] Nothing , Cmd.none)
                  (Ok s) -> (Loaded s Nothing , Cmd.none)

view : Model -> Html Msg
view model =
  case model of 
    Init -> div [] [ text "Hello World" ]
    (Loaded stations current) -> div [] (List.map stationToDiv stations)



type alias Country = { name : String , thumbnail : String }
type alias Station = 
  { name : String 
  , stream : String 
  , thumbnail : String 
  , country : Country
  , categories : List String
  }
stationToDiv : Station -> Html Msg 
stationToDiv station = 
  div []
      [ text station.name
      , br [] []
      , text station.stream
      , br [] []
      , text station.thumbnail
      , br [] []
      , img [ src station.thumbnail, width 100] []
      , img [ src station.country.thumbnail, width 100] []
      , audio [ src station.stream, controls True] []
      , br [] []
      ] 


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