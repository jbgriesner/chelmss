module Main exposing (..)
-- Make a GET request to load a book called "Public Opinion"
--
-- Read how it works:
--   https://guide.elm-lang.org/effects/http.html
--

import Browser
import Html exposing (Html, Attribute, div, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Html exposing (a)
import Html exposing (b)

main = Browser.sandbox {init = init, update = update, view = view}

type alias Model = {content: String}

init : Model
init = {content = ""}

type Msg = Change String

update : Msg -> Model -> Model
update msg model = 
  case msg of
    Change newContent -> {model | content = newContent}

view : Model -> Html Msg
view model = div [] 
  [ input [placeholder "Text to reverse", value model.content, onInput Change] [], 
    div [] [text (String.reverse model.content)]
  ]

type M a = T (a,a,a) | L (List a) | No

unlist : M a -> List a
unlist x = case x of
    L l -> l
    _ -> []

bind : M c -> (c -> M b) -> M b
bind u v = case u of
    No -> No
    T (h, i, j) -> case (v h, v i, v j) of
                    (T ((h1, h2, h3)), T ((i1, i2, i3)), T ((j1, j2, j3))) -> T (h1, i2, j3)
                    _ -> No
    L las -> let r = List.map v las 
                        |> List.map unlist |> List.concat 
             in L r
z = T (1, 2, 3)
y = L [1, 2, 3, 4]
q = No

uu : Int -> M Int
uu x = L [x]