# Introduction

A small, quick, lightweight, and fairly-well featured XML parser for Common Lisp. It has the following dependencies:

* [PARSE](http://github.com/massung/parse)
* [RE](http://github.com/massung/re)
* [LEXER](http://github.com/massung/lexer)
* [MARKUP](http://github.com/massung/markup)

## Quickstart

Once loaded you should be able to immediately begin parsing XML from both a string and a source file using `xml-parse` and `xml-load`.

    (xml-parse string &optional source)
    (xml-load pathname)

Let's give them a try...

    CL-USER > (xml-parse "<test>Hello, world!</test>")
    #<XML::XML-DOC "test">

In the test folder are a couple files you can try parsing as well.

    CL-USER > (xml-load #p"test/rss.xml")
    #<XML::XML-DOC "rss">

## Document Traversal

It should be easy enough (using the inspector) to see how the document and tags, attributes, etc. are laid out. But, there are a couple helper methods for finding and traversing the document:

    (xml-query [tag|doc] path &key all)  ;=> xml-tags

Querying is not an xpath, but it does allow for speedy finding of elements within a document. For example:

    CL-USER > (setf doc (xml-load #p"test/rss.xml"))
    #<XML::XML-DOC "rss">

    CL-USER > (xml-query doc "/*//item/title" :all t)
    (#<XML::XML-TAG "title">
     #<XML::XML-TAG "title">
     #<XML::XML-TAG "title">
     ...)

It should be noted that if the path begins with '/' then the document root is always the start of the search. Likewise, any subpath that is an empty string or "*" is considered a wildcard and will match all tags.

By default, the *all* parameter is NIL and only the first match will be returned. This is the first match in the tree as it appears in the document.

Additionally, there is `xml-query-attribute`, which can find an attribute from with a tag.

    (xml-query-attribute [tag|doc] attribute)

With both `xml-query` and `xml-query-attribute`, passing the document object will simply run the query on the root tag.

## What Does It Parse?

The XML package can parse *all* valid XML files.

Since it is non-validating, it will parse - but ignore - ELEMENT, ATTLIST, and NOTATION declarations in the DTD. It also ignores processing instructions. It does process ENTITY declarations, but will skip over parameter entity references in the DTD.

It will parse INCLUDE blocks of the DTD and ignore IGNORE blocks, but it since it doesn't expand parameter entity references, this really doesn't matter much.

Most importantly, it *will* track all external references in the DOCTYPE and ENTITY declarations (i.e. SYSTEM and PUBLIC references), and you have access to them, but it will not automatically resolve them.

## Exported Methods

The `XML` package is pretty sparse by design.

There are a couple functions for parsing from a source file or string, and searching a document for tags using a path. All other methods exposed are accessors in the `xml-doc`, `xml-node`, or a few other classes.

#### Parsing and Loading XML Files

    (xml-parse string &optional source)     ;=> xml-doc
    (xml-load pathname)                     ;=> xml-doc

#### Traverse/Query Methods

    (xml-query [tag|doc] path)              ;=> xml-tags
    (xml-query-attribute [tag|doc] name)    ;=> xml-attribute

#### `xml-doc` methods

    (xml-doc-source xml-doc)                ;=> pathname
    (xml-doc-version xml-doc)               ;=> string
    (xml-doc-encoding xml-doc)              ;=> keyword
    (xml-doc-standalone xml-doc)            ;=> boolean
    (xml-doc-root xml-doc)                  ;=> xml-tag
    (xml-doc-doctype xml-doc)               ;=> xml-doctype

#### `xml-ref` methods

    (xml-ref-public-id xml-external-ref)    ;=> string
    (xml-ref-system-uri xml-external-ref)   ;=> string

#### `xml-doctype` methods (subclass of `xml-ref`)

    (xml-doctype-root xml-doctype)          ;=> string
    (xml-doctype-entities xml-doctyoe)      ;=> xml-entities

#### `xml-node` methods

    (xml-node-doc xml-node)                 ;=> xml-doc
    (xml-node-name xml-node)                ;=> string
    (xml-node-value xml-node)               ;=> string

#### `xml-entity` methods (subclass of `xml-ref` and `xml-node`)

    (xml-entity-ndata xml-entity)           ;=> string

#### `xml-tag` methods (subclass of `xml-node`)

    (xml-tag-attributes xml-tag)            ;=> xml-attributes
    (xml-tag-elements xml-tag)              ;=> xml-tags

Attributes (`xml-attribute`) is a simple subclass of `xml-node` and has no special methods of its own.

That's it!

## Feedback

Something not working right? Have a question or comment? Feel free to email me or create an issue on [GitHub](http://github.com/massung/xml).
