-- Read all about this program in the official Elm guide:
-- https://guide.elm-lang.org/architecture/user_input/text_fields.html

import Html exposing (Html, Attribute, text, div, input, ruby, rt, button)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import String
import Char
import Dict exposing (member, get)
import Json.Decode exposing (dict, string)
import Http
import Array exposing (Array)

isChineseChar c =
  (c >= '\x4E00' && c <= '\x9FFF') ||
  (c >= '\x3400' && c <= '\x4DBF') ||
  (c >= '\x20000' && c <= '\x2A6DF') ||
  (c >= '\x2A700' && c <= '\x2B73F') ||
  (c >= '\x2B740' && c <= '\x2B81F') ||
  (c >= '\x2B820' && c <= '\x2CEAF') ||
  (c >= '\xF900' && c <= '\xFAFF') ||
  (c >= '\x2F800' && c <= '\x2FA1F')

{-
Block                                   Range       Comment
CJK Unified Ideographs                  4E00-9FFF   Common
CJK Unified Ideographs Extension A      3400-4DBF   Rare
CJK Unified Ideographs Extension B      20000-2A6DF Rare, historic
CJK Unified Ideographs Extension C      2A700–2B73F Rare, historic
CJK Unified Ideographs Extension D      2B740–2B81F Uncommon, some in current use
CJK Unified Ideographs Extension E      2B820–2CEAF Rare, historic
CJK Compatibility Ideographs            F900-FAFF   Duplicates, unifiable variants, corporate characters
CJK Compatibility Ideographs Supplement 2F800-2FA1F Unifiable variants
-}

type alias Model =
  { hzpydict : Dict.Dict String (List String)
  , inputbox : String
  , processedInfo : Array (String, Int, Int)
  }

type Msg = ReadDict (Result Http.Error (Dict.Dict String (List String)))
  | NewContent String
  | Reprocess
  | Switch Int

pinyinurl = "./output.json"

something : Http.Request (Dict.Dict String (List String))
something = Http.get pinyinurl (dict (Json.Decode.list string))

nth n xs = List.head (List.drop n xs)

match hzpydict i = flip get hzpydict >> Maybe.andThen (nth i) >> Maybe.withDefault ""

toRuby hzpydict n (c,i,l) = button [Switch n |> onClick] [ruby [] [c |> text, rt [] [match hzpydict i c |> text]], ruby [] [toString i |> text, rt [] [toString l |> text]]]
--onClick (Switch n)
getDict = Http.send ReadDict something

init : (Model, Cmd Msg)
init =
  ( Model Dict.empty "" Array.empty
  , getDict
  )

main = Html.program
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  }

numAlts hzpydict = flip get hzpydict >> Maybe.map List.length >> Maybe.withDefault 0
construct hzpydict x = (x, 0, numAlts hzpydict x)
modify (c,i,l) = (c,(i+1)%l,l)

updateArr n arr =
  case (Array.get n arr) of
    Nothing -> arr

    Just el -> Array.set n (modify el) arr

-- UPDATE
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ReadDict (Ok hzpydict) -> ({model | hzpydict = hzpydict}, Cmd.none)

    ReadDict (Err _) -> (model, Cmd.none)

    NewContent s -> {model | inputbox = s} |> update Reprocess

    Reprocess -> ({model | processedInfo = String.toList model.inputbox |> Array.fromList |> Array.map (String.fromChar >> construct model.hzpydict)}, Cmd.none)

    Switch n -> ({model | processedInfo = updateArr n model.processedInfo}, Cmd.none)

testFunc : Model -> List (Html Msg)
testFunc model = Array.indexedMap (toRuby model.hzpydict) model.processedInfo |> Array.toList

-- VIEW

view model =
  div []
    [ input [ placeholder "", onInput NewContent, myStyle ] []
    , testFunc model |> div [myStyle]
    ]

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

myStyle =
  style
    [ ("width", "100%")
    , ("height", "40px")
    , ("padding", "10px 0")
    , ("font-size", "2em")
    , ("text-align", "center")
    ]
