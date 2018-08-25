module Data.XML.Clark
  ( -- * EName
    EName
  , mkEName
  , mkUnprefixedEName
  , namespace
  , localPart

    -- * Namespace
  , Namespace
  , mkNamespace
  , namespaceURI
  , prefixHint
  , schemaLocationHint

  ) where

import Data.Text                 (Text)
import qualified Data.XML.Clark.Internal as I
import Data.XML.Clark.Internal  (Namespace, EName)

------------------------------------------------------------------------
-- EName

-- | Construct an 'EName'
mkEName :: Namespace
  -> Text -- ^ Local part, must be an XML NCName
  -> EName
mkEName ns localp = I.EName
  { I.namespace = Just ns
  , I.localPart = localp
  }

-- | For attributes without prefix and for unprefixed elements in documents
-- without default namespace.
mkUnprefixedEName :: Text -- ^ Local part, must be an XML NCName
  -> EName
mkUnprefixedEName localp = I.EName
  { I.namespace = Nothing
  , I.localPart = localp
  }

namespace :: EName -> Maybe Namespace
namespace = I.namespace

localPart :: EName -> Text
localPart = I.localPart

------------------------------------------------------------------------
-- Namespace

mkNamespace :: Text -- ^ URI
            -> Text -- ^ Prefix hint
            -> Maybe Text -- ^ Schema location hint
            -> Namespace
mkNamespace uri prefixhint lochint = I.Namespace
  { I.namespaceURI = uri
  , I.prefixHint = prefixhint
  , I.schemaLocationHint = lochint
  }

-- | The name of the namespace. Unlike the prefix, this is the
-- only thing a high level program should be concerned about.
namespaceURI :: Namespace -> Text
namespaceURI = I.namespaceURI

-- | A preferred prefix to use when serialising the document. This
-- can and will be safely ignored, but it helps to make your
-- documents more human-readable.
--
-- When two or more namespaces in a document have the same prefixHint,
-- similar prefixes should be chosen in a deterministic way, based
-- on the 'namespaceURI'
--
-- Leaving this empty should cause the document to have this
-- namespace as the default namespace, if possible.
prefixHint :: Namespace -> Text
prefixHint = I.prefixHint

-- | A optional location where an XML Schema for the namespace should be found.
schemaLocationHint :: Namespace -> Maybe Text
schemaLocationHint = I.schemaLocationHint
