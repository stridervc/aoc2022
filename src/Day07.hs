module Day07
  ( solve
  ) where

import qualified Text.Parsec as P
import Text.Parsec ((<|>))

import Data.List (find)

type DirPath  = [String]    -- ^ head of list is deepest dir
type FileName = String
type Size     = Int
data Entry    = File (FileName, Size) | Directory FileName [Entry] deriving (Eq, Show)

type ParseState = (DirPath, Entry)
type Parsed     = Entry
type Parser a   = P.Parsec String ParseState a  -- ^ New Parser type that includes state

-- | handle 'cd' commands
changePath :: FileName -> DirPath -> DirPath
changePath "/" _  = []
changePath ".." p = tail p
changePath n p    = n : p

-- | Get name of entry, regardless of whether it's a File or a Directory
filename :: Entry -> FileName
filename (File (name, _))   = name
filename (Directory name _) = name

-- | Non recursively find an entry by file name
findEntry :: FileName -> Entry -> Maybe Entry
findEntry name entry@(File (filename, _)) = if name == filename then Just entry else Nothing
findEntry name (Directory _ entries)      = find (\e -> filename e == name) entries

-- | insert or replace entry in a Directory
insertEntry :: Entry -> Entry -> Entry
insertEntry _ (File _)                      = error "Can't insert into a file"
insertEntry entry (Directory dname entries) = Directory dname (entry : filter (\e -> filename e /= filename entry) entries)

-- | add an entry to a directory with given path
addEntry :: Entry -> DirPath -> Entry -> Entry
addEntry entry [] tree  = insertEntry entry tree
addEntry entry ps tree  = case findEntry (last ps) tree of
  Nothing -> error $ "Error in addEntry, can't find " <> last ps
  Just e  -> insertEntry (addEntry entry (init ps) e) tree

-- | get file size, or sum of size of entries for directory
fileSize :: Entry -> Int
fileSize (File (_, size))       = size
fileSize (Directory _ entries)  = sum $ map fileSize entries

parseInt :: Parser Int
parseInt = do
  num <- P.many1 P.digit
  return $ read num

-- | parse directories and files, and add them to our tree
parseEntry :: Parser ()
parseEntry = P.choice
  [ do
    P.string "dir "
    name <- P.manyTill P.anyChar P.newline
    (path, tree) <- P.getState
    P.setState (path, addEntry (Directory name []) path tree)
  , do
    size <- parseInt
    P.space
    name <- P.manyTill P.anyChar P.newline
    (path, tree) <- P.getState
    P.setState (path, addEntry (File (name, size)) path tree)
  ]

-- | If we find a 'cd' command, update the current path
-- | Ignore 'ls' commands, we'll catch the file listing later
parseCommand :: Parser ()
parseCommand = do
  P.string "$ "
  P.choice  [ do
              P.string "cd "
              dir <- P.manyTill P.anyChar P.newline
              (path, tree) <- P.getState
              P.putState (changePath dir path, tree)
            , do
              P.string "ls"
              P.newline
              return ()
            ]

parse :: Parser ()
parse = do
  P.manyTill (P.choice [parseEntry, parseCommand]) P.eof
  return ()

-- | get sizes for all directories as a list of just the sizes
directorySizes :: Entry -> [Int]
directorySizes (File _) = []
directorySizes e@(Directory _ entries)  = fileSize e : concatMap directorySizes entries

part1 :: Parsed -> Int
part1 parsed = sum $ filter (<= 100000) $ directorySizes parsed

part2 :: Parsed -> Int
part2 parsed = minimum $ filter (>= needed) sizes
  where used    = fileSize parsed
        unused  = 70000000 - used
        needed  = 30000000 - unused
        sizes   = directorySizes parsed

solve :: String -> IO ()
solve input = do
  print $ part1 parsed
  print $ part2 parsed
  where Right (_, parsed) = P.runParser (parse >> P.getState) ([], Directory "/" []) "(input)" input
