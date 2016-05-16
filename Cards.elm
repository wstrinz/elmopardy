module Cards exposing (..)
import Types exposing (..)

genericCard : String -> Int -> Card
genericCard name value =
  { question = "what is the answer to this " ++ name ++ " card?"
  , answer = toString value ++ " " ++ name ++ "!"
  , value = value
  , state = Hidden }

placeholderCategory : String -> Category
placeholderCategory name =
  let
    generic = genericCard name
  in
    { name = name
    , cards =
      [
        generic 1000,
        generic 2500,
        generic 3000,
        generic 4000,
        generic 5000
      ]
    }

card : String -> String -> Int -> Card
card q a val =
  { question = q
  , answer = a
  , value = val
  , state = Hidden }

initialCatCards : List Card
initialCatCards =
  [
    card "who is the best cat of them all?" "Cucumber!" 1000,
    card "what breed of cat is cucumber" "British Shorthair (probably)" 2500,
    card "who invented the cat door" "Isaac Netwon" 3000,
    card "how long is the world's longest cat" "48.5 inches" 4000,
    card "which mexican city had a cat as mayoral candidate in 2013" "Mexico City" 5000
  ]
