slv "" ys = ("", "", ys)
slv xs "" = ("", xs, "")
slv xs'@(x : xs) ys'@(y : ys) =
    if x == y
        then
            let (a', b', c') = slv xs ys
                in (x : a', b', c')
        else ("", xs', ys')

main = do
    astr <- getLine
    bstr <- getLine
    let (a, b, c) = slv astr bstr
    putStrLn $ show (length a) ++ " " ++ a
    putStrLn $ show (length b) ++ " " ++ b
    putStrLn $ show (length c) ++ " " ++ c

