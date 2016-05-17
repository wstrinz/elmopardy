module Jeopardy exposing (..)

import Types exposing (..)
import Categories exposing (..)
-- import Cards exposing (..)
import List.Extra
import List exposing (map)
import Html exposing (div, button, text, tr, table, td, tbody, thead, th)
import Html.App as App
import Html.Events exposing (onClick)
import VirtualDom exposing (property, style)

-- Model Types
type alias Player = { name : String, score : Int }

type alias Model = { categories : List Category, players : List Player, currentPlayer : Player }

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
initialModel = { categories = initialCategories, players = initialPlayers, currentPlayer = initialPlayer }

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
      , playerDiv model
      ]

playerDiv : Model -> Html.Html Msg
playerDiv model =
  div []
  [ text "player: "
  , text <| toString model.currentPlayer
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
        QuestionShown -> card.question
        AnswerShown -> card.answer
        Finished -> "        "

    buttonText =
      case card.state of
        Hidden -> "Show"
        QuestionShown -> "Answer"
        Finished -> ""
        AnswerShown -> "Hide"

    cardAction =
      case card.state of
        Hidden -> [onClick <| ShowQ card]
        QuestionShown -> [onClick <| ShowA card]
        _ -> []

    cardButtons =
      case card.state of
        AnswerShown -> [ button [onClick <| Finish card True] [text "y"]
                    , button [onClick <| Finish card False] [text "n"]
                    ]
        _ -> [ button cardAction [text buttonText] ]

    cardInfo = [ text cardText
               , Html.br [] [] ]

  in
    td tdS <| List.append cardInfo cardButtons

categoryCell : Model -> Category -> Html.Html Msg
categoryCell model category =
  td []
    [ text category.name ]


-- update functions / types

type Msg =
  Increment (Player, Int) -- for testing; eventually remove
  | Decrement (Player, Int) --  in favor of 'Score'
  | ShowQ Card
  | Hide Card
  | ShowA Card
  | Finish Card Bool
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
      ShowQ card -> setState card QuestionShown
      Hide card -> setState card Hidden
      Finish card isCorrect -> simpleUpdate <| handlePlayerAnswer model card isCorrect
      ShowA card -> setState card AnswerShown
      Score _ -> simpleUpdate model


-- handleAnswer : Model -> Card -> Bool -> Model
-- handleAnswer model card isCorrect = handlePlayerAnswer model card isCorrect
handlePlayerAnswer : Model -> Card -> Bool -> Model
handlePlayerAnswer model card isCorrect =
  let
    equivCards ca cb = ca.question == cb.question && ca.value == cb.value
    checkCat category = {category | cards = List.map checkCard category.cards}
    scoreChange =
      if isCorrect then
        card.value
      else
        -1 * card.value

    checkCard c =
      if equivCards card c then
        {card | state = Finished }
      else
        c

    currPlayer = model.currentPlayer
    updatedPlayer = {currPlayer | score = model.currentPlayer.score + scoreChange }
    updatePlayer scanP =
      if model.currentPlayer == scanP then
        updatedPlayer
      else
        scanP
    in
      {model | categories = List.map checkCat model.categories
             , players = List.map updatePlayer model.players
             , currentPlayer = updatedPlayer }

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
