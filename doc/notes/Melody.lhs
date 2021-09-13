Melody
------
We will try and add some melodies here:



note   frequency 	period 	timeHigh
 c       261 Hz          3830 	 1915
 d       294 Hz          3400 	 1700
 e       329 Hz          3038 	 1519
 f       349 Hz          2864 	 1432
 g       392 Hz          2550 	 1275
 a       440 Hz          2272 	 1136
 b       493 Hz          2028	 1014
 C       523 Hz          1912 	 956

> data Notes = C | D | E | F | G | A | B | CC | Rest deriving (Ord, Show, Eq)

> freqMap :: [(Notes, Int)]
> freqMap = [ (C, 261)
>           , (D, 294)
>           , (E, 329)
>           , (F, 349)
>           , (G, 392)
>           , (A, 440)
>           , (B, 493)
>           , (CC, 523)
>           ]
>
> getFreq :: [(Notes, Int)] -> Notes -> Int
> getFreq _ Rest = 0
> getFreq [] _ = error "Note not found"
> getFreq ((x, y):xs) n
>   | n == x = y
>   | otherwise = getFreq xs n
>
> timePeriodUSec :: Int -> Int
> timePeriodUSec f = 1000000 `div` (2 * f)

Twinkle-twinkle little star


"ccggaag ffeeddc ggffeed ggffeed ccggaag ffeeddc " -- a space represents a rest

> twinkletwinkle = [C, C, G, G, A, A, G, F, F, E, E, D, D, C]
> beatLength = 500 -- msec

> notes = map f twinkletwinkle
>   where
>     f Rest = beatLength * 1000 -- usec
>     f x = timePeriodUSec $ freqMap `getFreq` x
