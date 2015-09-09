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

    CL-USER > (xml-query doc "/*//item/title")
    (#<XML::XML-TAG "title">
     #<XML::XML-TAG "title">
     #<XML::XML-TAG "title">
     ...)

It should be noted that if the path begins with '/' then the document root is always the start of the search. Likewise, any subpath that is an empty string or "\*" is considered a wildcard and will match all tags.

Additionally, there is `xml-query-attribute`, which can find an attribute from with a tag.

    (xml-query-attribute [tag|doc] attribute)

You can combine calls to `xml-query` and `xml-query-attribute` by appending `[@attribute]` to the end of the path you are querying. For example, in the following, example: XML document

    <people>
        <person name='bob'/>
        <person name='alice'/>
    </people>

To get the names of everyone, the following query can be used:

    (xml-query doc "//person@name")

This should return 2 attributes with the values `"bob"` and `"alice"`.

*Note: with both `xml-query` and `xml-query-attribute`, passing the document object will simply run the query on the root tag.*

## What Does It Parse?

The XML package can parse *all* valid XML files.

Since it is non-validating, it will parse - but ignore - ELEMENT, ATTLIST, and NOTATION declarations in the DTD. It also ignores processing instructions. It does process ENTITY declarations, but will skip over parameter entity references in the DTD.

Most importantly, it *will* track all external references in the DOCTYPE and ENTITY declarations (i.e. SYSTEM and PUBLIC references), and you have access to them, but it will not automatically resolve them.

It also correctly parses [XML namespaces](http://www.w3.org/TR/REC-xml-names/). However, a namespace *must* be declared for it to be considered valid. If an undeclared namespace is parsed, it will be skipped and the element will use the entire namespace+name for its name.

For example:

    <root>
        <items xmlns='default' xmlns:dc='dc'>
            <dc:item foo='bar' dc:baz='hello, world!'/>
            <undeclared:item/>
        </items>
    </root>

In the above example, the `xml-element-namespace` for the *root* tag will be NIL. For the *items* tag it will be the 'default' namespace. The first *item* tag it will be 'dc'. The *foo* attribute will also use the default namespace, and the *baz* attribute will use the 'dc' namespace. However, there is no 'undeclared' namespace, so the second *item* will actually be named *undeclared:item* and use the 'default' namespace.

## Querying with Namespaces

Querying for tags and attributes can also use namespaces. You can filter by namespace as well as wildcarding (empty name or "\*") using a namespace. Using the *rdf.xml* example in the test folder:

    CL-USER > (setf rdf (xml-load #p"test/rdf.xml"))
    #<XML-DOC "RDF">

    CL-USER > (xml-query rdf "//channel/*/rdf:/li@rdf:resource")
    (#<XML-ATTRIBUTE "resource">
     #<XML-ATTRIBUTE "resource">
     #<XML-ATTRIBUTE "resource">)

To quickly disect the example, it will...

1. ...start at the document root (since it's an absolute path),
2. ...match all tags,
3. ...then only match channel tags,
4. ...followed again by matching any tags,
5. ...then only tags from the 'rdf' namespace,
6. ...finally, all 'li' tags,
7. ...returning the 'resource' attributes in the 'rdf' namespace.

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

    (xml-node-name xml-node)                ;=> string
    (xml-node-value xml-node)               ;=> string

#### `xml-namespace` methods (subclass of `xml-node`)

    ;; no unique methods

#### `xml-entity` methods (subclass of `xml-ref` and `xml-node`)

    (xml-entity-ndata xml-entity)           ;=> string

#### `xml-element` methods (subclass of `xml-node`)

    (xml-element-ns xml-element)            ;=> xml-namespace
    (xml-element-doc xml-element)           ;=> xml-doc
    (xml-element-parent xml-element)        ;=> xml-element

#### `xml-tag` methods (subclass of `xml-element`)

    (xml-tag-namespaces xml-tag)            ;=> xml-namespaces
    (xml-tag-elements xml-tag)              ;=> xml-tags
    (xml-tag-attributes xml-tag)            ;=> xml-attributes

#### `xml-attribute` methods (subclass of `xml-element`)

    ;; no unique methods

That's it!

## Feedback

Something not working right? Have a question or comment? Feel free to email me or create an issue on [GitHub](http://github.com/massung/xml).
