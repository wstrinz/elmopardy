module Jeopardy exposing (..)

import Types exposing (..)
import Categories exposing (..)
import Cards exposing (..)
import List.Extra
import List exposing (map)
import Html exposing (div, button, text, tr, table, td, tbody, thead, th)
import Html.App as App
import Html.Events exposing (onClick)
import VirtualDom exposing (property, style)

-- Model Types
type alias Player = { name : String, score : Int }

type alias Model = { categories : List Category, players : List Player }

-- Entry Point
main : Program Never
main =
  App.program {
    init = init
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
  }


initialPlayer : Player
initialPlayer = { name = "Will", score = 0 }

initialPlayers : List Player
initialPlayers = [initialPlayer]

initialModel : Model
initialModel = { categories = initialCategories, players = initialPlayers }

init : ( Model, Cmd a )
init = ( initialModel, Cmd.none )

view : Model -> Html.Html Msg
view model =
  let
    player = (playerOrInit model)
  in
    div []
      [ -- dbgDiv model player ,
        div [] [ gameBoard model ]
      ]

gameBoard : Model -> Html.Html Msg
gameBoard model =
  let
    tableS = [ style [("border-collapse", "collapse")] ]
    tdS = [ style [("border", "1px solid #999")] ]
    categoryHeaders = map (\cat -> th tdS [text cat.name]) model.categories
    cardRows = List.Extra.transpose <| map .cards model.categories
    cardRow row = tr [] <| map cardCell row
    questionRows = map cardRow cardRows
  in
    div []
     [ table tableS
       [ thead [] [ tr [] categoryHeaders ]
       , tbody [] questionRows
       ]
     ]

cardCell : Card -> Html.Html Msg
cardCell card =
  let
    tdS = [ style [("border", "1px solid #999")] ]
    cardText =
      case card.state of
        Hidden -> toString card.value
        Focused -> card.question
        Answered -> card.answer
        Used -> "        "

    buttonText =
      case card.state of
        Hidden -> "Show"
        Focused -> "Answer"
        Used -> ""
        Answered -> "Hide"

    cardAction =
      case card.state of
        Hidden -> [onClick <| Show card]
        Focused -> [onClick <| Answer card True]
        Answered -> [onClick <| Finish card]
        _ -> []

    cardButtons =
      case card.state of
        Hidden -> [onClick <| Show card]
        Focused -> [onClick <| Answer card True]
        Answered -> [onClick <| Finish card]
        _ -> []

  in
    td tdS [ text cardText
           , Html.br [] []
           , button cardAction [text buttonText] ]

categoryCell : Model -> Category -> Html.Html Msg
categoryCell model category =
  td []
    [ text category.name ]


-- update functions / types

type Msg =
  Increment (Player, Int) -- for testing; eventually remove
  | Decrement (Player, Int) --  in favor of 'Score'
  | Show Card
  | Hide Card
  | Finish Card
  | Answer Card Bool
  | Score (Player, Card)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  let
    player = playerOrInit model
    incPlayer = {player | score = player.score + 1 }
    decPlayer = {player | score = player.score - 1 }
    setState card cardState = simpleUpdate <| setCardState model {card | state = cardState}
  in
    case msg of
      Increment (player, amt) ->
        simpleUpdate {model | players = [incPlayer]}
      Decrement (player, amt) ->
        simpleUpdate {model | players = [decPlayer]}
      Show card -> setState card Focused
      Hide card -> setState card Hidden
      Finish card -> setState card Used
      Answer card isCorrect -> handleAnswer model card isCorrect
      Score _ -> simpleUpdate model

handleAnswer : Model -> Card -> b -> ( Model, Cmd c )
handleAnswer model card isCorrect =
  simpleUpdate <| setCardState model {card | state = Answered}

setCardState : Model -> Card -> Model
setCardState model newCard =
  let
    equivCards ca cb = ca.question == cb.question && ca.value == cb.value
    checkCat category = {category | cards = List.map checkCard category.cards}
    checkCard card =
      if equivCards card newCard then
        newCard
      else
        card
  in
    {model | categories = List.map checkCat model.categories}


-- convenience methods
playerOrInit : { a | players : List Player } -> Player
playerOrInit model = Maybe.withDefault initialPlayer <| List.head model.players

simpleUpdate : Model -> ( Model, Cmd b )
simpleUpdate newModel = (newModel, Cmd.none)

cardRows : Model -> List (List Card)
cardRows model =
  List.Extra.transpose <| map .cards model.categories
