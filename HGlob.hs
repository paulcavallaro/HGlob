module HGlob (matchesPattern) where

type GlobPattern = String

{-- matchesPattern: Check to see if the provided String matches the provided GlobPattern --}
matchesPattern :: String -> GlobPattern -> Bool
matchesPattern text pattern
               | validPattern pattern = matchesPattern' text pattern
               | otherwise            = False

matchesPattern' :: String -> GlobPattern -> Bool
matchesPattern' (c:cs) ('?':ps)         = matchesPattern' cs ps
matchesPattern' str@(c:cs) pat@('*':ps) = or [(matchesPattern' cs ps),
                                              (matchesPattern' cs pat),
                                              (matchesPattern' str ps)]
matchesPattern' (c:cs) pat@('[':ps)        = let (matches, remPat) = charClass c pat
                                             in
                                             if matches then matchesPattern' cs remPat else False
matchesPattern' (c:cs) (p:ps)
                | c == p        = matchesPattern' cs ps
                | otherwise     = False
matchesPattern' "" ""           = True
matchesPattern' "" "*"          = True
matchesPattern' _ _             = False

charClass :: Char -> GlobPattern -> (Bool, GlobPattern)
charClass x ('[':'!':c:cs)       = let (elems, remPat) = charClass' [c] cs
                                 in (not (x `elem` elems), remPat)
charClass x ('[':c:cs)           = let (elems, remPat) = charClass' [c] cs
                                 in (x `elem` elems, remPat)
charClass x _                    = error "Invalid Character Class"

charClass' :: String -> GlobPattern -> (String, GlobPattern)
charClass' es (']':cs)  = (es, cs)
charClass' es (c:cs)    = charClass' (c:es) cs
charClass' _ _          = error "Unterminated Character Class"

{-- validPattern: Check to see if the provided GlobPattern is a valid one --}
validPattern :: GlobPattern -> Bool
validPattern _ = True
{--
validPattern "" = True
validPattern ('*':cs) = True
validPattern ('[':cs) = True
validPattern ('?':cs) = validPattern cs
validPattern (c:cs)   = validPattern cs
--}

validCharClass :: GlobPattern -> Bool
validCharClass _ = True
