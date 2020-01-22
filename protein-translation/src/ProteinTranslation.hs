module ProteinTranslation(proteins) where

translate :: String -> String
translate codon | codon == "AUG"              = "Methionine"
                | codon `elem` ["UUU", "UUC"] = "Phenylalanine"
                | codon `elem` ["UUA", "UUG"] = "Leucine"
                | codon `elem` ["UCU", "UCC", "UCA", "UCG"] = "Serine"
                | codon `elem` ["UAU", "UAC"] = "Tyrosine"
                | codon `elem` ["UGU", "UGC"] = "Cysteine"
                | codon == "UGG"              = "Tryptophan"
                | otherwise                   = "STOP"

proteins :: String -> Maybe [String]
proteins = Just . takeWhile (/= "STOP") . map translate . split

split :: String -> [String]
split ""         = []
split [_]        = []
split [_, _]     = []
split (a:b:c:xs) = (a : b : [c]) : split xs
