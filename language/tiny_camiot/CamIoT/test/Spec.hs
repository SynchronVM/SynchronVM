-- testing that the handwritten parser matches the generated one
main :: IO ()
main = putStrLn "not implemented"

{-
main :: IO ()
main = putStrLn "" >> sequence_ [
    runTest "../test/test0.cam",
    runTest "../test/test2.cam",
    runTest "../test/test3.cam",
    runTest "../test/test4.cam",
    runTest "../test/test5.cam",
    runTest "../test/test6.cam",
    runTest "../test/test7.cam",
    runTest "../test/test8.cam",
    runTest "../test/test9.cam",
    runTest "../test/test10.cam",
    runTest "../test/test11.cam",
    runTest "../test/test13.cam",
    runTest "../test/test14.cam",
    runTest "../test/test15.cam",
    runTest "../test/test16.cam",
    runTest "../test/test17.cam",
    runTest "../test/test19.cam",
    runTest "../test/test20.cam",
    runTest "../test/test21.cam",
    runTest "../test/test22.cam",
    runTest "../test/test23.cam",
    runTest "../test/test24.cam",
    runTest "../test/test25.cam",
    runTest "../test/test26.cam",
    runTest "../test/test27.cam",
    runTest "../test/test28.cam"]

runTest :: String -> IO ()
runTest input = do
    myp <- parseParser input
    bnfc <- parseBNFC input
    case bnfc of
        (Left e) -> error e
        (Right d) -> case myp of
            (Left e2) -> error e2
            (Right d') -> if d == d'
                    then putStrLn $ input ++ " OK"
                    else do
                        putStrLn "test FAILED"
                        putStrLn "********** BNFC **********"
                        putStrLn (printTree d)
                        putStrLn "\n\n"
                        putStrLn "********** My Parser **********"
                        putStrLn (printTree d')-}