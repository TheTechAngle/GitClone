{-# OPTIONS -Wall -fwarn-tabs #-}

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Objects (
   Object(..)
  ,ObjectId
  ,ObjectName
  ,EntryType(TTree, TBlob)
  ,TreeEntry
  ,Commit(parents, dateTime, tree)
  ,Blob(content)
  ,Tree(name, entries)
  ,makeCommit
  ,makeTree
  ,makeBlob
  ,toLineTree
  ,toLineBlob
  ,toLineCommit
  ,parseBlob
  ,parseCommit
  ,parseTree
  ,bytestr
  ,makeBlobEntryType
  ,makeTreeEntryType
  ,getTreeFromCommit
  ,getEntries
  ,getTreeName
  ,getAuthor
  ,getTree
  ,getParents
) where 
import qualified Data.ByteString.Char8 as C
import qualified Data.Attoparsec.ByteString.Char8 as PB
import Control.Applicative ((<|>))
import qualified Data.Time.Clock as DT
import qualified Data.Time.Format as DTF

-- | Definitions of Objects to be used including Blob,
-- | Commit, Tree and other supporting data structures.

type ObjectId = C.ByteString
type ObjectName = C.ByteString  
data EntryType = TTree | TBlob deriving (Eq)

instance Show EntryType where
  show TTree   = "tree"
  show TBlob   = "blob"

instance Ord EntryType where
  compare TTree TBlob = LT
  compare TBlob TTree = GT
  compare _ _         = EQ

data Object = CommitObj Commit | TreeObj Tree | BlobObj Blob deriving (Eq, Show)

data Commit = Commit {
  parents  :: [ObjectId]
 ,tree     :: ObjectId
 ,author   :: C.ByteString
 ,message  :: C.ByteString
 ,dateTime :: DT.UTCTime
} deriving (Show)

instance Eq Commit where
 (==) c1 c2 = (==) (dateTime c1) (dateTime c2)

instance Ord Commit where
  compare c1 c2 = compare (dateTime c2) (dateTime c1)

type TreeEntry = (EntryType, ObjectId, ObjectName)

data Tree = Tree{
 name :: C.ByteString,
 entries  :: [TreeEntry] -- same object id but different file names?
                                                 -- to prevent commit in tree
} deriving (Eq,Show)


data Blob = Blob{
 content :: C.ByteString
} deriving (Eq, Show)

-------------------------------------------------

getAuthor :: Commit -> C.ByteString
getAuthor = author

getTree :: Commit -> ObjectId
getTree = tree

getParents :: Commit -> [ObjectId]
getParents = parents

getTreeFromCommit :: Commit -> ObjectId
getTreeFromCommit = tree 

getEntries :: Tree -> [TreeEntry]
getEntries = entries

getTreeName :: Tree -> C.ByteString
getTreeName = name
-------------------------------------------------

-- | Constructors for other modules 

makeCommit :: [ObjectId]-> ObjectId -> C.ByteString -> C.ByteString -> 
              DT.UTCTime -> Object
makeCommit ps n a m t= CommitObj $ Commit ps n a m t

makeTree :: C.ByteString -> [TreeEntry] -> Object
makeTree n es = TreeObj $ Tree n es

makeBlob :: C.ByteString -> Object
makeBlob = BlobObj . Blob

makeTreeEntryType :: EntryType
makeTreeEntryType = TTree

makeBlobEntryType :: EntryType
makeBlobEntryType = TBlob
-------------------------------------------------
-- | Parsers for the three object types

bytestr :: String -> PB.Parser C.ByteString
bytestr s = PB.string $ C.pack s

constP :: String -> a -> PB.Parser a
constP s a = fmap (const a) (bytestr s)

nls :: PB.Parser C.ByteString
nls = PB.string (C.pack "\n") <|> PB.string (C.pack " ")

parseParent :: PB.Parser C.ByteString
parseParent = do
  pid   <- bytestr "parent " *> PB.takeTill (== '\n')
  _     <- nls
  pure pid

parseEntry :: PB.Parser (EntryType, ObjectId, ObjectName)
parseEntry = do
  etype  <- constP "blob " TBlob <|> constP "tree " TTree
  eid    <- PB.takeTill (== ' ')
  _      <- nls
  ename  <- PB.takeTill (== '\n') 
  _      <- nls
  pure (etype, eid, ename)

parseCommit :: PB.Parser Commit 
parseCommit = do
<<<<<<< HEAD
  cparents <- PB.many' parseParent
  ctree    <- bytestr "tree " *> PB.takeTill (== '\n')
  _        <- nls
  cauthor  <- bytestr "author " *> PB.takeTill (== '\n')
  _        <- nls
  _        <- nls
  cmsg     <- PB.takeTill (== '\n')
  _        <- nls
  ct       <- bytestr "time " *> PB.takeTill (== '\n')
  _        <- nls
  let cdateTime = DTF.parseTimeOrError True DTF.defaultTimeLocale "%s" (C.unpack ct)
  pure $ Commit cparents ctree cauthor cmsg cdateTime
=======
  parents <- PB.many' parseParent
  tree    <- bytestr "tree " *> PB.takeTill (== '\n')
  nls
  author  <- bytestr "author " *> PB.takeTill (== '\n')
  nls
  nls
  msg     <- PB.takeTill (== '\n')
  nls
  time    <- bytestr "time " *> PB.takeTill (== '\n')
  nls
  let dateTime = 
          DTF.parseTimeOrError True DTF.defaultTimeLocale "%s" (C.unpack time)
  pure $ Commit parents tree author msg dateTime
>>>>>>> aa5ba3b652ea584b6b48542ae433680489100c2b

parseTree :: PB.Parser Tree
parseTree = do
  tname    <- bytestr "name " *> PB.takeTill (== '\n')
  _        <- nls
  tentries <- PB.many' parseEntry
  pure $ Tree tname tentries



parseBlob :: PB.Parser Blob
parseBlob = do
  bcontent <- PB.takeByteString
  pure $ Blob bcontent
-------------------------------------------------

-- pretty printer for objects, for example, to write to files
toLineCommit :: Commit -> C.ByteString
toLineCommit c = Prelude.foldl (\b x -> b `C.append` helper "parent " x) 
                 (C.pack "") (parents c) 
                 `C.append` helper "tree " (tree c) 
                 `C.append` helper "author " (author c)
                 `C.append` helper "msg" (message c)
                 `C.append` helper "time " (C.pack (DTF.formatTime 
                                                      DTF.defaultTimeLocale 
                                                      "%s" (dateTime c)))
  where
    helper "msg" x    = C.pack "\n" `C.append` x `C.append` C.pack "\n"
    helper str x   = C.pack str `C.append` x `C.append` C.pack "\n"

toLineTree :: Tree -> C.ByteString 
toLineTree t = Prelude.foldl helper 
                (C.pack "name " `C.append` name t `C.append` C.pack "\n") 
                (entries t)
  where 
    helper base (x, hid, hname) = case x of
      TBlob -> base `C.append` C.pack "blob " `C.append` hid `C.append` 
               C.pack " " `C.append` hname `C.append` C.pack "\n"
      TTree -> base `C.append` C.pack "tree " `C.append` hid `C.append` 
               C.pack " " `C.append` hname `C.append` C.pack "\n"

toLineBlob :: Blob -> C.ByteString
toLineBlob = content
-------------------------------------

