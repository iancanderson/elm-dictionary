module App where

import Debug
import Html exposing (..)
import Html.Attributes exposing (class)
import Http
import Json.Decode exposing ((:=))
import Task exposing (Task, andThen)


-- MODEL

type alias ZipCode =
  { postCode: String
  }


zipCode : Json.Decode.Decoder ZipCode
zipCode =
  Json.Decode.object1 ZipCode
    ("post code" := Json.Decode.string)


type alias Model =
  ZipCode


init : Model
init =
  ZipCode "12345"


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

view : Model -> Html
view model =
  let th' field = th [] [text field]
      tr' zipCode = tr [] [ td [] [text <| zipCode.postCode]
                          ]
  in
    div [class "container"]
    [ table [class "table table-striped table-bordered"]
      [ thead [] [tr [] (List.map th' ["Post Code"])]
      , tbody [] [tr' model]
      ]
    ]
