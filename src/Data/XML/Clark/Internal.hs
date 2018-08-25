module Data.XML.Clark.Internal where

import Data.Function (on)
import Data.Text (Text)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

-- | An XML Namespace with a small bit of metadata to improve
-- serialisation.
data Namespace = Namespace
  { namespaceURI :: !Text
    -- ^ The name of the namespace. Unlike the prefix, this is the
    -- only thing a high level program should be concerned about.

  , prefixHint :: !Text
    -- ^ A preferred prefix to use when serialising the document. This
    -- can and will be safely ignored, but it helps to make your
    -- documents more human-readable.
    --
    -- When two or more namespaces in a document have the same prefixHint,
    -- similar prefixes should be chosen in a deterministic way, based
    -- on the 'namespaceURI'
    --
    -- Leaving this empty should cause the document to have this
    -- namespace as the default namespace, if possible.
    --
    -- This must a be an XML NCName

  , schemaLocationHint :: Maybe Text
    -- ^ A optional location where an XML Schema for the namespace should be found.
  }
  deriving (Show, Generic, Typeable)

-- | Equality is defined on the namespace URI Text alone
instance Eq Namespace where
  (==) = (==) `on` namespaceURI

-- | Ordering is defined on the namespace URI Text alone
instance Ord Namespace where
  compare = compare `on` namespaceURI

-- | An XML expanded name, consisting of an optional namespace and
-- local part.
--
-- This does not include the prefix, because prefixes only matter
-- somewhat when writing the document.
data EName = EName
  { namespace :: Maybe Namespace
  , localPart :: !Text -- ^ NCName
  }
  deriving (Eq, Ord, Show, Generic, Typeable)
