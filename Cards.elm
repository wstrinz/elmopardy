module Cards exposing (..)
import Types exposing (..)
import List.Extra

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
        generic 400,
        generic 800,
        generic 1200,
        generic 1600,
        generic 2000
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
    card "who is the best cat?" "Cucumber!" 400,
    card "what breed of cat is cucumber" "British Shorthair (probably)" 800,
    card "who invented the cat door" "Isaac Netwon" 1200,
    card "how long is the world's longest cat" "48.5 inches" 1600,
    card "which mexican city had a cat as mayoral candidate in 2013" "Mexico City" 2000
  ]

standardCards : List (String, String) -> List Card
standardCards cards =
  let
    cardFor (score, (question, answer)) = card question answer score
    baseScores = [400,800,1200,1600,2000]
  in
    List.map cardFor <| List.Extra.zip baseScores cards

initPrezCards : List Card
initPrezCards = standardCards [
  ("In 2016 he won a Grammy Award for the audio version of his memoir 'A Full Life: Reflections at Ninety'", "Jimmy Carter")
  , ("The face of this WWI-era prez was on the old $100,000 note, never used by the public", "Woodrow Wilson")
  , ("His memorial at Arlington mentions that he was both president & Chief Justice of the United States", "Taft")
  , ("He was the only president who had served as a 5-star general", "Eisenhower")
  , ("Millard Fillmore was the last member of this political party to serve as president", "Whig")]

zip : List a -> List b -> List (a,b)
zip xs ys =
  case (xs, ys) of
    ( x :: xs', y :: ys' ) ->
        (x,y) :: zip xs' ys'

    (_, _) ->
        []
