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
         | ChangeStation Station


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of 
    RequestStations -> (Init , Cmd.none)
    (RequestStationsResult res) -> 
      case res of (Err _) -> (Loaded {stations = [] , currentStation = Nothing} , Cmd.none)
                  (Ok s) -> (Loaded {stations = s , currentStation = Nothing} , Cmd.none)
    (ChangeStation station) ->
      case model of 
        (Loaded state) -> (Loaded {state | currentStation = Just station} , Cmd.none)
        _ -> (model , Cmd.none)

view : Model -> Html Msg
view model =
  case model of 
    Init -> div [] [ text "Radio World Loading" ]
    (Loaded data) -> viewApp data


viewApp : AppState -> Html Msg
viewApp state = 
  div [] 
      [ div [ class "navbar" ] [ h3 [] [ text "RADIO WORLD" ] ]
      , div [ class "sidebar" ]
            [ div [ class "country" ]
                  [ img [ src "https://upload.wikimedia.org/wikipedia/commons/thumb/7/73/Flag_of_Romania.svg/158px-Flag_of_Romania.svg.png" ]
                        []
                  , span [] [ text "Romania" ]
                  ]
            , div [ class "country" ]
                  [ img [ src "https://upload.wikimedia.org/wikipedia/en/thumb/a/a4/Flag_of_the_United_States.svg/220px-Flag_of_the_United_States.svg.png" ]
                        []
                  , span [] [ text "USA" ]
                  ]
            ]
      , div [ class "content" ]
            ([ div [ class "stations-filter" ]
                  [ div [ class "selected" ] [ text "ALL"]
                  , div [] [ text "FAVORITES (0)" ]
                  ]
            ] ++ (List.map viewStation state.stations) ++ [ viewPlayer state.currentStation ])
      , viewSidebar state.currentStation
      , node "link" [ href "https://fonts.googleapis.com/css2?family=Open+Sans:wght@300;400;600;700;800&display=swap" 
             , rel "stylesheet" ] []
      , node "link" [ href "https://visancosmin.github.io/Radio/main2.css" 
             , rel "stylesheet" ] [] 

      ]

viewStation : Station -> Html Msg 
viewStation station = 
  div [ class "station" ]
      [ img [ src station.thumbnail ] []
      , div [ class "details" ]
            [ h3 [ onClick (ChangeStation station)] [ text station.name ]
            , div [ class "tags" ] (List.map (\t -> span [] [text t]) station.categories)
            ]
      , div [ class "country" ]
            [ img [ src station.country.thumbnail ]
                  []
            , span [] [ text station.country.name ]
            ]
      ]

viewPlayer : Maybe Station -> Html Msg
viewPlayer maybeStation = 
  case maybeStation of 
    Nothing -> 
      div [ class "radio-station-player" ] 
          [ audio [ src "" , controls True , autoplay True] []
          ]
    (Just station) -> 
      div [ class "radio-station-player" ]
          [ audio [ src station.stream , controls True  , autoplay True] []
          ]

viewSidebar : Maybe Station -> Html Msg 
viewSidebar maybeStation = 
  case maybeStation of
    Nothing -> 
      div [ class "rightbar" ]
          [ p [ class "website-info" ] [ text "Radio World 2020"] 
          ]
    (Just station) -> 
      div [ class "rightbar" ]
          [ div [ class "station-info" ]
                [ div [ class "station-details" ]
                      [ img [ src station.thumbnail ] []
                      , div [ class "station-name" ]
                            [ h3 [] [ text station.name ]  
                            , div [ class "country" ]
                                  [ img [ src station.country.thumbnail ] []
                                  , span [] [ text station.country.name ]
                                  ]
                            ]
                      ]
                , a [ href station.website ] [ text station.website ]
                , p [] [ text station.description ]
                , div [ class "actions" ]
                      [ div [ class "favorite" ] [ text "❤ FAVORITE" ]
                      ]
                ]
          , p [ class "website-info" ] [ text "Radio World 2020"] 
          ]



type alias Country = { name : String , thumbnail : String }
type alias Station = 
  { name : String 
  , stream : String 
  , thumbnail : String 
  , country : Country
  , categories : List String
  , website : String 
  , description : String
  }

stationDecoder : Decoder Station 
stationDecoder = D.map7 Station
  (D.field "name" D.string)
  (D.field "stream" D.string)
  (D.field "thumbnail" D.string)
  (D.field "country" countryDecoder)
  (D.field "categories" (D.list D.string))
  (D.field "website" D.string)
  (D.field "description" D.string)

listDecoder : Decoder (List Station)
listDecoder = 
  D.field "stations" (D.list stationDecoder)

countryDecoder : Decoder Country
countryDecoder = D.map2 Country
  (D.field "name" D.string)
  (D.field "thumbnail" D.string)