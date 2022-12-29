{-# LANGUAGE LambdaCase #-}
module Day25 (solve) where

import Data.Function ( (&) )

solve input lines = do
    putStrLn $ foldl1 add lines
    putStrLn "Merry Christmas"

-- 'Tis the season for symbilic manipulation
add numA numB = do
    let len = max (length numA) (length numB)
    let zeros = repeat '0'
    let prep n = reverse n ++ zeros & take len
    let (a, b) = (prep numA, prep numB)
    foldl
        (\ (acc, carry) (digA, digB) -> do
                let [digCarry, digSum] = addDigits digA digB
                let [carCarry, carSum] = addDigits digSum carry
                let [_, carry'] = addDigits digCarry carCarry
                (carSum : acc, carry'))
        ("", '0')
        (zip a b)
    & \ case (acc, '0') -> acc; (acc, c) -> c : acc

addDigits a b =
    case (a, b) of
        ('2', '2') -> "1-"
        ('2', '1') -> "1="
        ('2', '0') -> "02"
        ('2', '-') -> "01"
        ('2', '=') -> "00"

        ('1', '2') -> "1="
        ('1', '1') -> "02"
        ('1', '0') -> "01"
        ('1', '-') -> "00"
        ('1', '=') -> "0-"

        ('0', '2') -> "02"
        ('0', '1') -> "01"
        ('0', '0') -> "00"
        ('0', '-') -> "0-"
        ('0', '=') -> "0="

        ('-', '2') -> "01"
        ('-', '1') -> "00"
        ('-', '0') -> "0-"
        ('-', '-') -> "0="
        ('-', '=') -> "-2"

        ('=', '2') -> "00"
        ('=', '1') -> "0-"
        ('=', '0') -> "0="
        ('=', '-') -> "-2"
        ('=', '=') -> "-1"
