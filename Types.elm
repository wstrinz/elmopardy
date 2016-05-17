module Types exposing (..)

type CardState = Hidden | QuestionShown | AnswerShown | Finished
type alias Card = { question : String , answer : String , value : Int , state : CardState }
type alias Category = { name : String, cards : List Card }
