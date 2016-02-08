module App where

import Debug
import Html exposing (..)
import Html.Attributes exposing (class)
import Http
import Json.Decode exposing ((:=))
import Task exposing (Task, andThen)
import String


-- MODEL

type alias Place =
  { placeName: String
  }

type alias ZipCode =
  { postCode: String
  , places: List Place
  }

place : Json.Decode.Decoder Place
place =
  Json.Decode.object1 Place
    ("place name" := Json.Decode.string)

zipCode : Json.Decode.Decoder ZipCode
zipCode =
  Json.Decode.object2 ZipCode
    ("post code" := Json.Decode.string)
    ("places" := (Json.Decode.list place))


type alias Model =
  ZipCode


init : Model
init =
  ZipCode "12345" [Place "Nowhere"]


-- UPDATE

type Action
  = NoOp
  | SetZipCode (ZipCode)


update : Action -> Model -> Model
update action model =
  case action of
    NoOp ->
      model

    SetZipCode model' ->
      model'


-- SIGNALS

main : Signal Html
main =
  Signal.map view model


model : Signal Model
model = Signal.foldp update init actions.signal


actions : Signal.Mailbox Action
actions =
  Signal.mailbox NoOp


get : Task Http.Error (ZipCode)
get =
  Http.get zipCode "http://api.zippopotam.us/us/02148"


port runner : Task Http.Error ()
port runner =
  get `andThen` (SetZipCode >> Signal.send actions.address)


-- VIEW

placeName : Place -> String
placeName place = place.placeName

zipCodeDisplay : ZipCode -> String
zipCodeDisplay zipCode =
  let places = String.join "," <| List.map placeName zipCode.places
  in zipCode.postCode ++ places

view : Model -> Html
view model =
  let th' field = th [] [text field]
      tr' zipCode = tr [] [ td [] [text <| zipCodeDisplay zipCode]
                          ]
  in
    div [class "container"]
    [ table [class "table table-striped table-bordered"]
      [ thead [] [tr [] (List.map th' ["Post Code"])]
      , tbody [] [tr' model]
      ]
    ]
