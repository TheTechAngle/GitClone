{-# OPTIONS -fwarn-tabs -fwarn-incomplete-patterns  #-}
{-# OPTIONS -Wall -fwarn-tabs #-}

{-# LANGUAGE PackageImports        #-}

module ObjectStore(
  Repo
 ,Branch
 ,Ref
 ,RefStore
 ,getRefs
 ,addRef
 ,lookupRef
 ,createRef
 ,getObjPath
 ,exportObject
 ,readObject
 ,hashContent
  ) where
import "cryptohash" Crypto.Hash
import Data.ByteString.Char8 as C
import qualified Objects as O
import System.FilePath
import Control.Monad ()
import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString.Char8 as PB
import Data.Map as Map

-- | Ref store maps branch names to their latest commit ids
type RefStore  = Map Ref O.ObjectId
type Ref = C.ByteString
type Repo = String
type Branch = String

addRef :: RefStore -> Ref -> O.ObjectId -> RefStore
addRef refs branch objId = Map.insert branch objId refs 

getRefs :: RefStore -> [(C.ByteString, O.ObjectId)]
getRefs = Map.toList

lookupRef :: C.ByteString -> RefStore -> Maybe O.ObjectId
lookupRef = Map.lookup

createRef :: RefStore
createRef = Map.empty 
-- Given the name of the repository and id, this gives you the filepath
getObjPath :: Repo -> O.ObjectId -> FilePath
getObjPath r o = r </> ".hit" </> "objects"  </> Prelude.take 2 (C.unpack o) </> Prelude.drop 2 (C.unpack o)

hexSha256 :: ByteString -> ByteString
hexSha256 bs = digestToHexByteString (hash bs :: Digest SHA256)

-- | Given object, adds header and hashes to give id and new content

hashContent :: O.Object-> (O.ObjectId, C.ByteString)
hashContent obj = do
  let content          = objToByte obj
      header           = getHeader obj ((show . C.length) content)
      headerAndContent = header `C.append` content
      hid               = hexSha256 headerAndContent
  (hid, headerAndContent) 
  where 
      getHeader (O.CommitObj _) l = createHeader "commit" l 
      getHeader (O.TreeObj _) l   = createHeader "tree" l
      getHeader (O.BlobObj _) l   = createHeader "blob" l 
      createHeader objectType l = C.pack (objectType ++ " ") `C.append` C.pack (l ++ "\0")

-- | Consolidates a object type to a bytestring
objToByte :: O.Object -> C.ByteString
objToByte (O.CommitObj c) = O.toLineCommit c
objToByte (O.TreeObj t)   = O.toLineTree t
objToByte (O.BlobObj b)   = O.toLineBlob b

-- | Gets the id, filepath and content in bytestrings of an object
exportObject :: Repo -> O.Object -> (FilePath, O.ObjectId, C.ByteString)
exportObject r obj= do
  let (xid, content) = hashContent obj
      path  = getObjPath r xid
  (takeDirectory path, xid, content)


parseHeader :: String -> Parser ByteString
parseHeader str = O.bytestr str *> takeTill (=='\0') <* char '\0'

parseObject :: Parser O.Object
parseObject = parseHeader "commit" *> fmap O.CommitObj O.parseCommit <|> 
              parseHeader "tree" *> fmap O.TreeObj O.parseTree <|> 
              parseHeader "blob" *> fmap O.BlobObj O.parseBlob

readObject :: ByteString -> Maybe O.Object
readObject str = case parseOnly parseObject str of
  Right obj -> Just obj
  _         -> Nothing

 

-------------------------------------------------------------------





