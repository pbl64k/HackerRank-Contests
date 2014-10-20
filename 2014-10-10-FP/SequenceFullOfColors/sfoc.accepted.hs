upd (r, g, y, b) 'R' = (succ r, g, y, b)
upd (r, g, y, b) 'G' = (r, succ g, y, b)
upd (r, g, y, b) 'Y' = (r, g, succ y, b)
upd (r, g, y, b) 'B' = (r, g, y, succ b)

slv (r, g, y, b) [] = r == g && y == b
slv (r, g, y, b) (c : cs) =
    if abs (r' - g') > 1 || abs (y' - b') > 1
        then False
        else slv acc cs
    where
        acc@(r', g', y', b') = upd (r, g, y, b) c

tst = do
    cs <- getLine
    putStrLn $ show $ slv (0, 0, 0, 0) cs

main = do
    tstr <- getLine
    mapM_ (const tst) [1 .. read tstr]

