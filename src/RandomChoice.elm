module RandomChoice exposing (..)


import Browser
import Html exposing (Attribute, Html, button, div, input, text, ul, li, br, h5, h6, p, hr)
import Html.Attributes exposing (type_, class, placeholder, value, readonly)
import Html.Events exposing (keyCode, on, onClick, onInput)
import Json.Decode as Json
import Random


-- MAIN


main =
    Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view 
    }



-- MODEL


type alias Model =
  { choices: List String
  , presetMagicEightBall: List String
  , presetFood: List String
  , newChoice: String
  , chosen: String
  }


init : () -> (Model, Cmd Msg)
init _ =
  ( Model [] 
     [ "As I see it, yes"
     , "Ask again later"
     , "Better not tell you now"
     --, "Cannot predict now"
     --, "Concentrate and ask again"
     , "Don’t count on it"
     --, "It is certain"
     --, "It is decidedly so"
     , "Most likely"
     --, "My reply is no"
     , "My sources say no"
     , "Outlook not so good"
     , "Outlook good"
     --, "Reply hazy, try again"
     --, "Signs point to yes"
     , "Very doubtful"
     , "Without a doubt"
     --, "Yes"
     --, "Yes – definitely"
     --, "You may rely on it"
     ]
     [ "\u{1F354}" -- Burger
     , "\u{1F961}" -- Chinese takeout
     , "\u{1F96A}" -- Sandwich
     , "\u{1F355}" -- Pizza
     , "\u{1F363}" -- Sushi
     , "\u{1F32E}" -- Taco
     , "Stay home and cook \u{1F627}"
     ]
     ""
     ""
  , Cmd.none
  )
  


-- UPDATE


type Msg
  = NewChoice String
  | AddChoice
  | RemoveChoice String
  | Choose
  | Chosen String
  | ClearAll
  | PresetFood
  | PresetMagicEightBall

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NewChoice newChoice ->
      ( { model | newChoice = newChoice }
      , Cmd.none
      )

    AddChoice -> 
      if not (String.isEmpty model.newChoice) && not (List.member model.newChoice model.choices) then
        ( { model | choices = model.choices ++ [ model.newChoice ]
        , newChoice = ""
        , chosen = ""
        }
        , Cmd.none
        )
      else
        ( model
        , Cmd.none
        )
    
    RemoveChoice choice ->
      ( { model | choices = removeChoice model.choices choice }
      , Cmd.none
      )

    Choose ->
      ( model
      , Random.generate Chosen (Random.uniform (Maybe.withDefault "" (List.head model.choices)) (Maybe.withDefault [""] (List.tail model.choices)))
      )

    Chosen chosen ->
      ( { model | chosen = chosen }
      , Cmd.none
      )

    ClearAll ->
      ( { model | choices = []
        , newChoice = ""
        , chosen = ""
      }
      , Cmd.none
      )

    PresetFood ->
      ( { model | choices = model.presetFood 
        , newChoice = ""
        , chosen = ""
        }
      , Cmd.none
      )

    PresetMagicEightBall ->
      ( { model | choices = model.presetMagicEightBall 
        , newChoice = ""
        , chosen = ""
        }
      , Cmd.none
      )
      

removeChoice : List String -> String -> List String
removeChoice choices choice =
  List.filter (\c -> c /= choice) choices

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW


view : Model -> Html Msg
view model =
  div [ class "container" ]
    [ div [ class "pt-4" ]
      [ h5 [] [ text "Let Me Choose that for You!" ]
      , hr [] []
      ]
    , h6[] [ text "Choices:" ]
    , div [ class "btn-group" ]
        [ viewButton "btn btn-secondary" ClearAll "Clear All"
        , viewButton "btn btn-secondary" PresetFood "Food"
        , viewButton "btn btn-secondary" PresetMagicEightBall "Magic 8-Ball"
        ]
    , ul [ class "list-group" ] 
      [ viewChoices model.chosen model.choices
      , viewNewChoice model.newChoice
      ]
    , br [] []
    , div [ class "form-group" ]
        [ viewButton "btn btn-primary mb-4" Choose "Choose!"
        ]
    ]

viewButton : String -> Msg -> String -> Html Msg
viewButton cls toMsg txt = 
  button [ type_ "button", class cls, onClick toMsg ] [ text txt ]

viewChoices : String -> List String -> Html Msg
viewChoices chosen choices =
  div [] (List.map (viewChoice chosen) choices)

viewChoice : String -> String -> Html Msg
viewChoice chosen choice =
  li ([] ++ (choiceClasses chosen choice))
    [ 
      viewButton "btn btn-secondary mr-2" (RemoveChoice choice) "X"
    , text choice
    ]

choiceClasses : String -> String -> List (Attribute msg)
choiceClasses chosen choice = 
  [ class "list-group-item" ] ++
    if choice == chosen then
      [ class "chosen" ]
    else
      []

viewNewChoice : String -> Html Msg
viewNewChoice s =
  li [ class "list-group-item" ] 
    [ input [type_ "text", class "form-control", placeholder "Type a new choice and hit Enter", value s, onInput NewChoice, onEnter AddChoice ] []
    ]

-- CUSTOM EVENT HANDLERS
onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Json.succeed msg
            else
                Json.fail "not ENTER"
    in
        on "keydown" (Json.andThen isEnter keyCode)