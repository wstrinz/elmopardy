module Categories exposing (..)
import Types exposing (..)
import Cards exposing (initialCatCards)

placeholderCategory : String -> Category
placeholderCategory name =
  let
    genericCard value =
      { question = "what is the answer to this " ++ name ++ " card?"
      , answer = toString value ++ " " ++ name ++ "!"
      , value = value
      , state = Hidden }
  in
  { name = name
  , cards =
    [
      genericCard 1000,
      genericCard 2500,
      genericCard 3000,
      genericCard 4000,
      genericCard 5000
    ]
  }

initialCategories : List Category
initialCategories = [ {name = "Cats", cards = initialCatCards}
                    , placeholderCategory "boots"
                    , placeholderCategory "foo"
                    , placeholderCategory "bar"
                    , placeholderCategory "borges" ]
