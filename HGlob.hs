module HGlob (matchesPattern) where

type GlobPattern = String

{-- matchesPattern: Check to see if the provided String matches the provided GlobPattern --}
matchesPattern :: String -> GlobPattern -> Bool
matchesPattern text pattern
               | validPattern pattern = matchesPattern' text pattern
               | otherwise            = False

matchesPattern' :: String -> GlobPattern -> Bool
matchesPattern' (c:cs) ('?':ps)         = matchesPattern' cs ps
matchesPattern' (c:cs) ('\\':p:ps)
                | c == p                = matchesPattern' cs ps
                | otherwise             = False
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
charClass x pat@('[':'!':cs)       = let (elems, remPat) = charClass' "" pat
                                 in (not (x `elem` elems), remPat)
charClass x pat@('[':cs)           = let (elems, remPat) = charClass' "" pat
                                 in (x `elem` elems, remPat)
charClass x _                    = error "Invalid Character Class"

charClass' :: String -> GlobPattern -> (String, GlobPattern)
charClass' es ('[':'!':cs)  = charClass' es cs
charClass' es ('[':cs)      = charClass' es cs
charClass' es (']':cs)  = (es, cs)
charClass' es (n:'-':m:cs) = charClass' ([n..m] ++ es) cs
charClass' es ('\\':c:cs) = charClass' (c:es) cs
charClass' es (c:cs)    = charClass' (c:es) cs
charClass' _ _          = error "Unterminated Character Class"

alphabet = ['a'..'z']

{-- validPattern: Check to see if the provided GlobPattern is a valid one --}
validPattern :: GlobPattern -> Bool
validPattern _ = True

validCharClass :: GlobPattern -> Bool
validCharClass _ = True
