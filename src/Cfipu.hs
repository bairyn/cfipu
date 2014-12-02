--8000---

{-
cfipu is a primitive, minimalistic language, similar to assembly and brainfuck.  It can read input from stdin, and print to stdout.  When a file is interpreted, its contents are placed in memory after being processed by the preprocessor.
There are two pointers: the data pointer, and the instruction pointer, both of which initially point to the first byte.  Each byte is parsed, the instruction pointer is set ahead by one byte, and then the command is executed if the byte is a command.  There are only 8 primitive commands:

0: End the program.
1: Print current byte.
2: Get one character (blocking, if necessary).  If EOF has been reached, the data pointer is set behind by one.
3: Set the data pointer behind by one.
4: Set the data pointer ahead by two.
5: Decrement the value pointed to by the data pointer by one.
6: If the current byte is non-zero, the data pointer is set behind by one; otherwise, the data pointer is set forward by two.
7: Sets the instruction pointer to the data pointer.

When 0x00 or 0x30 is read at the instruction pointer, the program is ended; and so forth.  If the byte read at the instruction pointer is not recognized, it is simply ignored, and the program continues as usual.

The memory will expand as much your system will allow.  Every byte is 0x00, by default.  Every cell in memory is a byte, and can only have 256 values: 0x00-0xFF.

Additionally, a preprocessor exists so that common code can be factored out.
#: This symbol is place before each symbol that should not be replaced or removed (it can be placed before another hash).  These symbols are not treated specially *inside* comments, particularly delimited ones.
@: Delimited macro.  This is simply a preprocessor shortcut, and it can also be used for readability.  The length of the delimiter is the sum of one and the predefined symbols following the symbol.  So "@0000macro07macro macro" evaluates to " 07".  Macros can also be used for better readability.  If a '@' symbol appears within the body of the definition of a macro, it will be treated like any other non-command symbol.  (The preprocessor removes the macro definitions and replaces the instances of the them with the body of them).  Comments are removed by the preprocessor before macros are parsed.  Macros are parsed in one pass entirely by the preprocessor before replacement actually takes place, so the relative positioning of the macros from the rest of the source doesn't affect the functionality of the program, although the order of macro definitions themselves does matter, since a macro can appear in the definition of another one that is defined later in the program.
8: This begins a delimited comment.  The end delimiter can be elided at the end of a file.
9: This is the other type of comments.  When this is read by the preprocessor, any whitespace preceding the '9' is removed as well as the rest of the line, but not including the newline character, which may be omitted, in which case the rest of the file is removed by the preprocessor.

Incrementing the data pointer by two might seem arbitrary, but is necessary, because otherwise it would be impossible to set the data pointer ahead of the instruction pointer.

After macros are parsed by the preprocessor, all unknown characters are stripped out by the preprocessor *until* the special character sequence @@ is encountered.  The @@ is removed by the preprocessor and then it isn't treated specially anymore, and then characters that would not be recognized by the instruction pointer are no longer stripped out.

Any file can be interpreted or compiled as cfipu source; but, of course, not all cfipu programs will run as intended.  They can run indefinitely, or maliciously use up many resources.
-}

{-
 - Example program
 -
 - Take 2 single digit numbers, and print the difference also as a single digit.  This program assumes that both digits are single digit numbers and that the result is also a single digit number.

---

@-5-
@+---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
@<3<
@>43>

@0n+++++++++++++++++++++++++++++++++++++++++++++++++n+
@0n-------------------------------------------------n-
9 @0n+n+
9 @0n+n+

8*
@0w0n+w0
@0w1n++w1
@0w2n+++w2
@0w3n++++w3
@0w<w3w<
@0w4n+++++w4
@0w5n++++++w5
@0w-w5w-
@0w6n+++++++w6
@0w7n++++++++w7

@0u0n-u0
@0u1n--u1
@0u2n---u2
@0u3n----u3
@0u<u3u<
@0u4n-----u4
@0u5n----=-u5
@0u-u5u-
@0u6n-------u6
@0u7n--------u7
*

@0w0w0
@0w1+w1
@0w2++w2
@0w3+++w3
@0w<w3w<
@0w4++++w4
@0w5+++++w5
@0w-w5w-
@0w6++++++w6
@0w7+++++++w7

@0u0u0
@0u1-u1
@0u2--u2
@0u3---u3
@0u<u3u<
@0u4----u4
@0u5---=-u5
@0u-u5u-
@0u6------u6
@0u7-------u7

@z>w<>w->w6>w4>w7<<<<7z 9 Set the value in the data pointer to zero.  The 5 bytes after it must be zero; they will not be zero after execution (although nothing prevents them from being reset to zero; this function doesn't need to be called because the bytes will always be set to the same commands.  Execution is returned to 6 bytes after the original value.
9@s>>w3>w3>w5>w4>w3>w5>w6>w4>w7<<<<<<<<7s 9 Subtract the current byte by the byte in the cell to the right.  9 bytes after the two cells must be zero initially and will be changed.  Execution is returned to 11 bytes after the current cell.
@s>>w3>w3>w5>w4>w3>w5>w6>w4>w7<<<<<<<<7s 9 Subtract the current byte by the byte in the cell to the right.  9 bytes after the two cells must be zero initially and will be changed.  Execution is returned to 11 bytes after the current cell.

@0uz>u<>u4>u6>u4>u7<<<<<uz 9 Set the data pointer to what it was set to when you called the original function.  After this is finished executing, The data pointer will be set to what is was immediately before this function was called.
@0us>>u3>u3>u5>u4>u2>u4>u6>u4>u7<<<<<<<<us

9 Note that we have two 5's due to the comment at the beginning.  This is unavoidable if we use '-' as a macro.

9 Allocate 64 bytes of data
<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
2>2n-<
>>>>>>>>>>>
w<>w<>w<>w<>w<>w<>w<>w<>w<>w<>w<>w1>w0><<<<<<<<<<<<<
<<<<<<<<<<<
s
80000000000
-}

--- MAIN ---

{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}

module Main
  ( main
  , help
  , execute
  , preprocess
  , isPredefinedSymbol
  , Format
  , setWidth
  , factor
  , PMacro
  , PMName
  , PMPattern
  , Pattern
  , macroPatterns
  , macroPatternsMaxKeyLen
  , pmacrosFull
  , pmacros
  , findIndexPrefix
  , mlookupLen
  , lookupLen
  , flp
  , maybeOutify
  , foldrUpdate
  , apply
  , wrap

  , module Data.Memory
  ) where

import Control.Exception
import Control.Monad.State
import Data.Char (isSpace)
import Data.Default
import Data.List (genericLength, genericIndex, genericTake, genericDrop, genericSplitAt, findIndex, intersperse, isPrefixOf, sortBy, sort)
import qualified Data.Map as M
import Data.Memory
import qualified Data.Set as S
import System.Environment (getArgs, getProgName)
import System.IO (openFile, hGetContents, IOMode(..), hSetBuffering, BufferMode(..), stdin, stdout)
import System.IO.Error

main :: IO ()
main = do
    args     <- getArgs
    progName <- getProgName
    let argc = genericLength args

    if null args
        then do
            help progName
        else do
            let src  = args !! 0
                pre  = args !! 1
                fact = args !! 2
                wdth = args !! 3
            handle <- openFile src ReadMode
            source <- hGetContents handle

            source `seq` return ()

            let format = flip execState def $ do
                    when (argc >= 4) $ do
                        setWidth $ read wdth
                preprocessed = preprocess source
                facpreprocessed = factor format preprocessed

            when (argc >= 2) $ do
                writeFile pre preprocessed


            when (argc >= 3) $ do
                writeFile fact facpreprocessed

            let mem = initialMemory . stringToCells $ preprocessed
            execute mem

help :: String -> IO ()
help progName = do
    putStrLn $ "Usage: " ++ progName ++ " [IN source filename] (OUT preprocessed source filename) (OUT factored preprocessed source filename) (width of line wrapping for factoring)"
    putStrLn $ "All written files will be overwritten without warning."

--- EXECUTION ---

execute :: Memory -> IO ()
execute m = do
    hSetBuffering stdin  NoBuffering
    hSetBuffering stdout NoBuffering
    execute' m
    where execute' :: Memory -> IO ()
          execute' m = let m' = moveInstructionPointerRight m
                       in  case M.lookup (getInstruction m) instructions of
                               (Just instruction) -> instruction m' execute'
                               (Nothing)          -> execute' m'

instructions :: M.Map Cell (Memory -> (Memory -> IO ()) -> IO ())
instructions = M.fromList . concat . (\ ~(is:iss) -> map (\ ~(k, v) -> (k - (charToCell '0'), v)) is : iss) . replicate 2 . map (\ ~(k, v) -> (charToCell k, v)) $
    [ ('0', \m r -> do
                return ())
    , ('1', \m r -> do
                putChar $ cellToChar $ getData m
                r m)
    , ('2', \m r -> do
                c <- getChar
                r $ setData (charToCell c) m
                ; `catch` get m r)
    , ('3', \m r -> do
                r $ moveDataPointerLeft m)
    , ('4', \m r -> do
                r $ moveDataPointerRight . moveDataPointerRight $ m)
    , ('5', \m r -> do
                r $ modifyData dec m)
    , ('6', \m r -> do
                let c  = getData m
                    c' = cellToWord8 c
                case c' of
                    0x00 -> r $ moveDataPointerRight . moveDataPointerRight $ m
                    _    -> r $ moveDataPointerLeft m)
    , ('7', \m r -> do
                  r $ moveInstructionPointerToDataPointer m)
    ]
    where dec :: Cell -> Cell
          dec c = let c' = cellToWord8 c
                  in  case c' of
                          0x00 -> 0xFF
                          x    -> pred x
          get m r e
              | isEOFError e = r $ moveDataPointerLeft m

--- THE PREPROCESSOR ---

preprocess :: String -> String
preprocess = removeHashes . parseRecognizedCharacters . parseMacros . parseLineComments . parseDelimitedComments

removeHashes :: String -> String
removeHashes [] = []
removeHashes ('#':x:xs) = x : removeHashes xs
removeHashes (x:xs)     = x : removeHashes xs

parseRecognizedCharacters :: String -> String
parseRecognizedCharacters []           = []
parseRecognizedCharacters ('#':x:xs)   = '#':x : parseRecognizedCharacters xs
parseRecognizedCharacters ('@':'@':xs) = xs
parseRecognizedCharacters (x:xs)
    | isPredefinedSymbol x = x : parseRecognizedCharacters xs
    | otherwise            = parseRecognizedCharacters xs

parseLineComments :: String -> String
parseLineComments = concatMap line . lines
    where line :: String -> String
          line xs = let xs' = replace xs
                        replace :: String -> String
                        replace []         = []
                        replace ('#':x:xs) = '#':'#' : replace xs
                        replace (x:xs)     = x : replace xs
                    in  case (findIndex isLineCommentSymbol xs') of
                            (Just i)  -> let i' = b $ pred i
                                             b i
                                                 | i < 0             = 0
                                                 | isSpace $ xs !! i = b $ pred i
                                                 | otherwise         = succ i
                                         in  genericTake i' xs
                            (Nothing) -> xs ++ "\n"

isLineCommentSymbol :: Char -> Bool
isLineCommentSymbol '9' = True
isLineCommentSymbol _   = False

parseDelimitedComments :: String -> String
parseDelimitedComments = parse
    where parse  :: String -> String
          ignore :: String -> String -> String
          parse  []           = []
          parse  ('#':x:xs)   = '#':x : parse xs
          parse  ('8':xs)     = let lenStr           = takeWhile isPredefinedSymbol xs
                                    len              = genericLength lenStr
                                    rest             = genericDrop len xs
                                    (former, latter) = genericSplitAt (succ len) rest
                                in  ignore former latter
          parse  (x:xs)       = x : parse xs
          ignore _         []         = []
          ignore delimiter a@(_:xs)
              | genericTake (genericLength delimiter) a == delimiter = parse $ genericDrop (genericLength delimiter) a
              | otherwise                                     = ignore delimiter xs

-- macros --

-- We need an *ordered* map
type Macros = [(PMName, PMPattern)]

parseMacros :: String -> String
parseMacros = execState $ do
    m  <- readMacros
    m' <- processMacros m
    applyMacros m'

-- Read macros and remove their definitions
readMacros :: State String Macros
readMacros = state $ step False []
    where step :: Bool -> Macros -> String -> (Macros, String)
          step _      ms  []          = (ms, [])
          step noIgnr ms ('#':x:xs)   = let (m', s') = step noIgnr ms xs
                                        in  (m', '#':x : s')
          step False  ms ('@':'@':xs) = let (m', s') = step True ms xs
                                        in  (m', '@':'@' : s')
          step noIgnr ms ('@':xs)     = let lenStr           = takeWhile isPredefinedSymbol xs
                                            len              = genericLength lenStr
                                            rest             = genericDrop len xs
                                            (name, body)     = genericSplitAt (succ len) rest
                                            terminatorIndex  = findIndexPrefix name body
                                            (former, latter) = genericSplitAt terminatorIndex body
                                            macro            = (name, former)
                                            post             = genericDrop (succ len) latter
                                        in  step noIgnr (macro : ms) post
          step noIgnr ms (x:xs)       = let (m', s') = step noIgnr ms xs
                                        in  (m', x : s')

-- Substitute macro definitions themselves, and sort the list of macros by length of key, descending, so that the macros with longer names will be tested first.
processMacros :: Macros -> State String Macros
processMacros ms = return . fst . foldrUpdate replace 0 $ ms
    where len = genericLength ms
          replace x (lastMacros, n) = let n' = succ n
                                          replaceSingle (name, pattern) = (name, execState (applyMacros [x]) pattern)
                                      in  ((map replaceSingle $ genericTake (len - n') lastMacros) ++ (genericDrop (len - n') lastMacros), n')

applyMacros :: Macros -> State String ()
applyMacros [] = return ()
applyMacros m  = modify step
    where m'              = sortBy sort m
          len             = maximum . map (genericLength . fst) $ m'
          sort a b        = (genericLength . fst $ b) `compare` (genericLength . fst $ a)
          step []         = []
          step ('#':x:xs) = '#':x : step xs
          step a@(x:xs)   = case lookupLen (genericTake len a) $ m' of
                                (Just (pattern, len)) -> pattern ++ step (genericDrop len a)
                                (Nothing)             -> x : step xs

isPredefinedSymbol :: Char -> Bool
isPredefinedSymbol '0' = True
isPredefinedSymbol '1' = True
isPredefinedSymbol '2' = True
isPredefinedSymbol '3' = True
isPredefinedSymbol '4' = True
isPredefinedSymbol '5' = True
isPredefinedSymbol '6' = True
isPredefinedSymbol '7' = True
isPredefinedSymbol '8' = True
isPredefinedSymbol '9' = True
isPredefinedSymbol '@' = True
isPredefinedSymbol '#' = True
isPredefinedSymbol _   = False

--- FACTORING ---

-- May not work correctly when the input is changed after being preprocessed
factor :: Format -> String -> String
factor fmt s = let s'            = prefixHash s
                   (ms, s'')     = apply (genericLength macroPatterns) S.empty s'
                   apply :: (Integral a) => a -> S.Set PMacro -> String -> (S.Set PMacro, String)
                   apply 0 ms xs = (ms, xs)
                   apply n ms xs = let (ms', xs') = r' ms xs
                                   in  apply (pred n) ms' xs'
                   body          = s''
                   nms           = pmacros S.\\ ms
                   ms'           = reverse . sort $ S.toList ms
                   nms'          = reverse . sort $ S.toList nms
                   header        = initl ++ foldr used [] ms' ++ inter ++ foldr unused [] nms' ++ append
                   used   :: PMacro -> String -> String
                   used   x acc  = (++) acc $ case lookup x pmacrosFull of
                                                  (Just (name, pattern)) -> "@" ++ (replicate (pred . genericLength $ name) '0') ++ name ++ pattern ++ name ++ "\n"
                                                  (Nothing)              -> []
                   unused :: PMacro -> String -> String
                   unused = used
                   initl  = ""
                   inter
                       | null nms' = ""
                       | otherwise = "\n80--\n\n"
                   append
                       | null nms' = ""
                       | otherwise = "--\n\n"
                   text = (++) header $ flip wrap body $ f_width fmt
               in  text
    where r' :: S.Set PMacro -> String -> (S.Set PMacro, String)
          r' ms []       = (ms, [])
          r' ms a@(x:xs) = case lookupLen (genericTake macroPatternsMaxKeyLen a) macroPatterns of
                               (Just ((Pattern {p_macro = macro, p_name = name}), len)) -> let (ms', a') = r' (S.insert macro ms) $ genericDrop len a
                                                                                           in  (ms', name ++ a')
                               (Nothing)                                                -> let (ms', xs') = r' ms $ xs
                                                                                           in  (ms', x:xs')

prefixHash :: String -> String
prefixHash []       = []
prefixHash a@(x:xs) =
    case lookupLen (take macroPatternsMaxKeyLen a) $ map snd pmacrosFull of
        (Just (_, len)) -> ('#' : intersperse '#' (genericTake len a)) ++ (prefixHash $ genericDrop len a)
        (Nothing)       -> x : prefixHash xs

data PMacro =  -- predefined macro enumeration
    PM_minus       |
    PM_plus        |
    PM_lessthan    |
    PM_greaterthan |
    PM_nminus      |
    PM_nplus deriving (Eq, Ord)

type PMName    = String
type PMPattern = String

data Pattern = Pattern { p_macro :: PMacro
                       , p_name  :: PMName
                       }

macroPatterns ::[(PMPattern, Pattern)]
macroPatterns = map (\ ~(pattern, (name, macro)) -> (pattern, Pattern {p_name = name, p_macro = macro})) $
    [ ("5",               ("-",  PM_minus))
    , (replicate 255 '-', ("+",  PM_plus))
    , ("3",               ("<",  PM_lessthan))
    , ("43",              (">",  PM_greaterthan))
    , (replicate 48 '-',  ("n-", PM_nminus))
    , (replicate 48 '+',  ("n+", PM_nplus))
    ]
macroPatternsMaxKeyLen :: (Integral a) => a
macroPatternsMaxKeyLen = maximum . map genericLength . map fst $ macroPatterns

pmacrosFull :: [(PMacro, (PMName, PMPattern))]
pmacrosFull = map f macroPatterns
    where f (pattern, (Pattern {p_macro = macro, p_name = name})) = (macro, (name, pattern))

pmacros :: S.Set PMacro
pmacros = S.fromList . map fst $ pmacrosFull

-- formatting --

data Format = Format { f_width :: Integer
                     }
instance Default Format where
    def = Format { f_width = 80
                 }

setWidth :: Integer -> State Format ()
setWidth w = modify $ (\ ~fmt -> fmt{f_width = w})

--- HELPER FUNCTIONS ---

findIndexPrefix :: forall a b. (Eq a, Eq b, Integral a) => [b] -> [b] -> a
findIndexPrefix find = r' 0
    where r' :: (Eq a, Eq b, Integral a) => a -> [b] -> a
          r' i [] = i
          r' i a@(_:xs)
              | find `isPrefixOf` a = i
              | otherwise           = r' (succ i) xs

mlookupLen :: (Eq k, Ord k, Integral b) => [k] -> M.Map [k] a -> Maybe (a, b)
mlookupLen k = lookupLen k . sortBy (\ ~(k, v) ~(k2, v2) -> genericLength k2 `compare` genericLength k) . M.toList

lookupLen :: (Eq k, Integral b) => [k] -> [([k], a)] -> Maybe (a, b)
lookupLen []       _  = Nothing
lookupLen a@(_:ks) xs = foldr (\ ~(k, v) acc -> if k `isPrefixOf` a then Just (v, genericLength k) `mplus` acc else Nothing `mplus` acc) mzero xs --`mplus` lookupLen ks xs

flp :: (a, b) -> (b, a)
flp (a, b) = (b, a)

maybeOutify :: ((Maybe a), b) -> Maybe (a, b)
maybeOutify ((Just a),  b) = Just (a, b)
maybeOutify ((Nothing), _) = Nothing

foldrUpdate :: (a -> ([a], b) -> ([a], b)) -> b -> [a] -> ([a], b)
foldrUpdate = r' 0
    where r' :: (Integral c) => c -> (a -> ([a], b) -> ([a], b)) -> b -> [a] -> ([a], b)
          r' n f z xs = let (xs', acc) = r' (succ n) f z xs
                        in  case () of _
                                           | n >= genericLength xs -> (xs, z)
                                           | otherwise             -> f (genericIndex xs' n) (xs', acc)

-- Negative integers are not checked!
apply :: (Integral a) => a -> (b -> b) -> b -> b
apply 0 f = id
apply n f = f . apply (pred n) f

wrap :: forall a. (Integral a) => a -> String -> String
wrap w = r' 0
    where r' :: (Integral a) => a -> String -> String
          r' a []        = ""
          r' a ('\n':xs) = '\n' : r' 0 xs
          r' a s@(x:xs)
              | not $ isPredefinedSymbol x = '@':'@':s
              | a >= w                     = x:'\n' : r' 0 xs
              | otherwise                  = x : (r' (succ a) xs)
