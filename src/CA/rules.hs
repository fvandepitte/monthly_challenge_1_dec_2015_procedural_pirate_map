module CA.Rules where

import CA (Rule, Cell)

islandRule :: Rule
islandRule (c, alive) cs | length (filterAlive cs) > 4          = (c, True)
                         | length (filterAlive cs) > 3 && alive = (c, True)
                         | otherwise                            = (c, False)

filterAlive :: [Cell] -> [Cell]
filterAlive = filter (id . snd)