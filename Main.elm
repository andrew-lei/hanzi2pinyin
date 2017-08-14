-- Read all about this program in the official Elm guide:
-- https://guide.elm-lang.org/architecture/user_input/text_fields.html

import Html exposing (Html, Attribute, text, div, input)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import String
import Char
import Dict exposing (member, get)
import Json.Decode exposing (dict, string)
import Http

test = List.range (Char.toCode '\x4DF0') (Char.toCode '\x4E20') |> List.map Char.fromCode |> String.fromList
--'\x9FFF'
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
  }

type Msg = ReadDict (Result Http.Error (Dict.Dict String (List String)))
  | NewContent String

pinyinurl = "./output.json"

something : Http.Request (Dict.Dict String (List String))
something = Http.get pinyinurl (dict (Json.Decode.list string))

getDict = Http.send ReadDict something

init : (Model, Cmd Msg)
init =
  ( Model (Dict.empty) ""
  , getDict
  )

bool2char b = if b then '1' else '0'
bool2str b = if b then "1 " else "0 "

main = Html.program
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  }


-- UPDATE
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ReadDict (Ok hzpydict) -> ({model | hzpydict = hzpydict}, Cmd.none)

    ReadDict (Err _) -> (model, Cmd.none)

    NewContent s -> ({model | inputbox = s}, Cmd.none)

--testFunc = String.concat << List.map (bool2str << isChineseChar) << String.toList
testFunc : Model -> String
testFunc model = (String.concat << List.map (String.fromChar >> flip get model.hzpydict >> Maybe.andThen List.head >> Maybe.withDefault "") << String.toList) model.inputbox


-- VIEW

view model =
  div []
    [ input [ placeholder "", onInput NewContent, myStyle ] []
    , div [ myStyle ] [ text (testFunc model) ]
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
