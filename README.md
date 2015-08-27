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

## What Does XML Parse?

The XML package can parse *all* valid XML files. While the parse is non-validating, it does do the following (in addition to typical parsing of tags):

* DOCTYPE and DTD
* ENTITY tags
* Parameter ENTITY tags
* Loading SYSTEM references
* Proper expansion of entities!
* Embedding of parameter entities within the DTD
* Processing instructions

Special attention should be noted on "proper expansion of entities". Many smaller XML parsers do not do this opting to completely ignore the DTD all-together. For example, many XML parsers will get this wrong (taken from [http://www.w3.org/TR/REC-xml/](http://www.w3.org/TR/REC-xml/)):

    <!DOCTYPE tricky [
    <!ENTITY % xx '&#37;zz;'>
    <!ENTITY % zz '&#60;!ENTITY tricky "error-prone">'>
    %xx;
    ]>
    <tricky>This is &tricky;...</tricky>

The above should contain a tag (tricky) with the inner text value "This is error-prone...". Note the use of parameter entities, proper expansion (at the proper time), recursive expansion, and expansion within the DTD.

Additionally, processing instructions are parsed and kept with either the document or the tag (if parsed from within a tag), and can be viewed afterwards.

## Exported Methods

The `XML` package is pretty sparse by design. There are a couple functions for parsing from a source file or string, and searching a document for tags using a path. All other methods exposed are accessors in the `xml-doc`, `xml-node`, or a few other classes.

#### Parsing and Loading XML Files

    (xml-parse string &optional source)              ;=> xml-doc
    (xml-load pathname)                              ;=> xml-doc

#### Traverse/Query Methods

    (xml-query xml-tag path &key all)                ;=> xml-tags
    (xml-query xml-doc path &key all)                ;=> xml-tags

    (xml-query-attribute xml-tag name)               ;=> xml-attribute
    (xml-query-attribute xml-doc name)               ;=> xml-attribute

#### `xml-doc` methods

    (xml-doc-source xml-doc)                         ;=> pathname
    (xml-doc-version xml-doc)                        ;=> string
    (xml-doc-encoding xml-doc)                       ;=> keyword
    (xml-doc-standalone xml-doc)                     ;=> boolean
    (xml-doc-root xml-doc)                           ;=> xml-tag
    (xml-doc-doctype xml-doc)                        ;=> xml-doctype
    (xml-doc-entities xml-doc)                       ;=> xml-entities
    (xml-doc-instructions xml-doc)                   ;=> xml-processing-instructions

#### `xml-external-ref` methods

    (xml-external-ref-public-id xml-external-ref)    ;=> string
    (xml-external-ref-system-uri xml-external-ref)   ;=> string

#### `xml-doctype` methods (subclass of `xml-external-ref`)

    (xml-doctype-root xml-doctype)                   ;=> string

#### `xml-node` methods

    (xml-node-doc xml-node)                          ;=> xml-doc
    (xml-node-name xml-node)                         ;=> string
    (xml-node-value xml-node)                        ;=> string

#### `xml-tag` methods (subclass of `xml-node`)

    (xml-tag-attributes xml-tag)                     ;=> xml-attributes
    (xml-tag-elements xml-tag)                       ;=> xml-tags
    (xml-tag-instructions xml-tag)                   ;=> xml-processing-instructions

Processing instructions and attributes are simple subclasses of `xml-node`. The entity class is both a subclass of `xml-node` and `xml-external-ref`.

That's it!

## Feedback

Something not working right? Have a question or comment? Feel free to email me or create an issue on [GitHub](http://github.com/massung/xml).
