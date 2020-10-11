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


type Model = Init | Loaded (List Station)

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
      case res of (Err _) -> (Loaded [] , Cmd.none)
                  (Ok s) -> (Loaded s , Cmd.none)

view : Model -> Html Msg
view model =
  case model of 
    Init -> div [] [ text "Hello World" ]
    (Loaded res) -> div [] (List.map stationToDiv res)




type alias Station = { name : String , stream : String , thumbnail : String }
stationToDiv : Station -> Html Msg 
stationToDiv station = 
  div []
      [ text station.name
      , br [] []
      , text station.stream
      , br [] []
      , text station.thumbnail
      , br [] []
      , br [] []
      ] 


stationDecoder : Decoder Station 
stationDecoder = D.map3 Station
  (D.field "name" D.string)
  (D.field "stream" D.string)
  (D.field "thumbnail" D.string)

listDecoder : Decoder (List Station)
listDecoder = 
  D.field "stations" (D.list stationDecoder)