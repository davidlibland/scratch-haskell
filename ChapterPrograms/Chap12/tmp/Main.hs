module Main where
import Parsing
import Expressions
import Laws
import Calculations

simplify :: [String] -> String -> Calculation
simplify strings string
    = let laws = map (parse law) strings
          e = parse expr string
       in calculate laws e

prove :: [String] -> String -> Calculation
prove strings string
    = let laws = map (parse law) strings
          (e1, e2) = parse equation string
       in paste (calculate laws e1) (calculate laws e2)

laws1 = ["defn pruneBy: pruneBy f = f . map pruneRow . f",
         "expand after boxs: expand . boxs = map boxs . expand",
         "filter with boxs: filter (p . boxs) = map boxs . filter p . map boxs",
         "boxs involution: boxs . boxs = id",
         "map functor: map f . map g = map (f.g)",
         "map functor: map id = id",
         "defn expand: expand = cp . map cp",
         "filter after cp: filter (all p) . cp = cp . map (filter p)",
         "property of pruneRow: filter nodups . cp . pruneRow = filter nodups . cp",
          "hack: map boxs . cp . map cp = cp . map cp . boxs"]

main :: IO ()

main = do {
    x <- getLine;
    if x /= "quit"
    then do {
        putStrLn $ show $ (parse equation x);
        putStrLn $ show $ prove laws1 x;
        main
    }
    else return ()
}