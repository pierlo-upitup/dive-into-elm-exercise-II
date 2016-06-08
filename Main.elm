module Main exposing (..)

-- --------------------------
-- Exercise 1:
-- --------------------------
-- Update the project to 0.17
-- Compile and check if everything works as expected.
--
-- https://github.com/elm-lang/elm-platform/blob/master/upgrade-docs/0.17.md
--
-- --------------------------
-- Exercise 2 (optional):
-- --------------------------
-- Download the Closure Compiler and figure out how it works. Then:
-- 1. Compile the elm project to a JS file (use the --output=filename.js flag)
-- 2. Compile the resulting JS file with the Closure Compiler in ADVANCED mode
--
-- https://developers.google.com/closure/compiler/
--
-- --------------------------
-- Exercise 3 (optional):
-- --------------------------
-- Create an outgoing port that exposes the current forecast. Create an
-- index.html and embed the elm application in fullscreen mode. Log the
-- received forecast-data to the browser console.
-- Start elm reactor and check if everything works as exptected.
--
-- http://guide.elm-lang.org/interop/javascript.html


import Html exposing (..)
import Html.Events exposing (onClick)
import Html.App as Html
import Time exposing (..)

-- MODEL


type alias Weather =
  { day : String, max : Int, min : Int, description : String }


type alias City =
  String


type alias Model =
  { city : City, forecast : List Weather }



-- forecast model data of Hamburg and Berlin


forecast_hh : Model
forecast_hh =
  { city = "Hamburg"
  , forecast =
      [ { day = "TUE", max = 19, min = 8, description = "Light Rain" }
      , { day = "WED", max = 15, min = 4, description = "Mostly Sunny" }
      , { day = "THU", max = 17, min = 6, description = "Sunny" }
      , { day = "FRI", max = 20, min = 10, description = "Sunny" }
      , { day = "SAT", max = 22, min = 11, description = "Sunny" }
      , { day = "SUN", max = 22, min = 12, description = "Sunny" }
      ]
  }


forecast_berlin : Model
forecast_berlin =
  { city = "Berlin"
  , forecast =
    [ { day = "TUE", max = 18, min = 7, description = "Partly Cloudy" }
    , { day = "WED", max = 16, min = 4, description = "Cloudy" }
    , { day = "THU", max = 19, min = 6, description = "Sunny" }
    , { day = "FRI", max = 21, min = 10, description = "Mostly Sunny" }
    , { day = "SAT", max = 23, min = 11, description = "Mostly Sunny" }
    , { day = "SUN", max = 24, min = 12, description = "Mostly Sunny" }
    ]
  }



-- empty model data


initalModel : Model
initalModel =
  forecast_hh



-- UPDATE


type Msg
  = Reset
  | Show Model
  | Toggle
  | Tick Time


update : Msg -> Model -> (Model, Cmd a)
update action model =
  case action of
    Reset ->
      (initalModel, Cmd.none)

    Show forecast ->
      (forecast, Cmd.none)

    Toggle ->
      if model == forecast_hh then
        (forecast_berlin, Cmd.none)
      else
        (forecast_hh, Cmd.none)

    Tick time -> 
      let cur = Debug.log "time: " time
      in (model, Cmd.none)



-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every second Tick

-- VIEW


forecastListView : List Weather -> Html Msg
forecastListView forecast =
  let
    -- table header
    header =
      tr
        []
        [ th [] [ text "Day" ]
        , th [] [ text "Max" ]
        , th [] [ text "Min" ]
        , th [] [ text "Detail" ]
        ]

    -- table content
    list =
      case forecast of
        [] ->
          []

        detail ->
          List.map forecastDetailView detail
  in
    table
      []
      [ thead
          []
          [ header ]
      , tbody
          []
          list
      ]


forecastDetailView : Weather -> Html Msg
forecastDetailView detail =
  tr
    []
    [ td [] [ text detail.day ]
    , td [] [ text (toString detail.max ++ "°C") ]
    , td [] [ text (toString detail.min ++ "°C") ]
    , td [] [ text detail.description ]
    ]


cityView : City -> Html Msg
cityView city =
  h1 [] [ text city ]


view : Model -> Html Msg
view model =
  div
    []
    [ button
        [ onClick Toggle ]
        [ text "Toggle" ]
    , button
        [ onClick Reset ]
        [ text "Reset" ]
    , cityView model.city
    , forecastListView model.forecast
    ]




main =
  Html.program
    { init = (initalModel, Cmd.none)
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
