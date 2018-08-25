# xml-clark

*High-level XML Namespace handling for Haskell.*

This library is named after James Clark, for his work on XML and, in particular, to piggyback on *Clark notation*. Clark look like this:

    {http://www.cars.com/xml}part

so only the namespace and local name, a combination also known as [EName](https://www.w3.org/TR/REC-xml-names/#dt-expname). This is part of the canonical representation of anything that's built on the [XML Namespace recommendation](https://www.w3.org/TR/REC-xml-names/#dt-localname).

# No QName

Many XML libraries use QName instead, which is an EName with a prefix, but that representation tends to be problematic, because prefixes are inherently tied to the location in their document. This is a problem because this prevents you from freely taking XML from document and using it in another - ruining composability.

> Processors conforming to this specification MUST recognize and act on these declarations and prefixes.

*[The XML Namespaces recommendation](https://www.w3.org/TR/REC-xml-names/#dt-qualname) on qualified names*

This library attempts to take that sentence to its logical conclusion: where possible, avoid exposing them to applications.

# Purpose of this library

This library provides an `EName` type for other libraries to use and share.
It is not an XML processing library by itself.

XML processing can and should be done in many ways. Having a single `EName` type makes it easier to use more than one XML processing library.

# Design of this library

The non-internal interface of this library is restrictive, in order to allow more freedom to optimise performance in future versions.

# How to use this library

Make `EName` and `Namespace` part of your favorite XML processing library.

Declare top-level `Namespace`s and `EName`s for readability (and probably better performance).

# To Do

 - [ ] Add supporting functions to help out with serialisation
    - [ ] makePrefixesUnique :: [EName] -> Map EName Text
    - [ ] some code to help out with name -> ename conversion in a document,
          tracking namespace scope etc
 - [ ] Integrate with at least one XML processing library

# Further work

Optimisations to consider:
 - Store bytestring versions of the local part and prefix hint for an efficient ByteString API
    - Maybe use thunks again for either or both?
    - Compare performance with bytestring-only version
 - EName [interning](https://en.wikipedia.org/wiki/String_interning)

# Shoutouts

 - Thanks to Chris de Vreeze for sharing his insights with me in the past and sharing his excellent [yaidom](https://github.com/dvreeze/yaidom) Scala library with the world!
