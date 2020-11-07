module RandomChoice exposing (..)


import Browser
import Html exposing (Attribute, Html, button, div, input, text, ul, li, br, h2, h4, p, hr)
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
  , newChoice: String
  , chosen: String
  }


init : () -> (Model, Cmd Msg)
init _ =
  ( Model [ "Burgers", "Hoagies", "Pizza", "Sushi", "Tacos", "Cook tonight and make a big mess and have to do dishes and all that" ] "" ""
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
      [ h2 [] [ text "LMCTFY!" ]
      , p [] [ text "Let me choose that for you!  Enter some choices and click Choose!" ]
      , hr [] []
      ]
    , h4 [] [ text "Choices:" ]
    , div [ class "form-group" ]
        [ viewButton "btn btn-secondary" ClearAll "Clear All"
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
    [ input [type_ "text", class "form-control", placeholder "New choice ...", value s, onInput NewChoice, onEnter AddChoice ] []
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