module Main (main) where
import Lib

main :: IO ()
main = do
    let unsortedList = [9, 3, 7, 2, 1, 1, 8, 4, 6, 5, 0, 3, 2, 7, 8, 9, 5, 4, 6, 0]
    let sortedList = quicksort unsortedList

    putStrLn "Unsorted list: "
    print unsortedList

    putStrLn "Sorted list: "
    print sortedList

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
-- Deel de lijst op in een pivot (eerste element/head) en de rest van de lijst (de tail)
quicksort (pivot:remaining) =
    -- Sorteer recursief alle elementen voor de pivot
    let smallerSorted = quicksort [element | element <- remaining, element <= pivot]
    -- Sorteer recursief alle elementen na de pivot
        largerSorted = quicksort [element | element <- remaining, element > pivot]
    -- Voeg de gesorteerde lijsten en pivot samen
    in smallerSorted ++ [pivot] ++ largerSorted