# Introduction

A small, quick, lightweight, and fairly-well featured XML parser and query language for SBCL. It has the following dependencies:

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

## Querying

It should be easy enough (using the inspector) to see how the document and tags, attributes, etc. are laid out. But, for more complicated document and node traversal, it is recommended to use the `xml-query` function:

    (xml-query [tag|doc] [query|string])  ;=> results

A query is extremely similar to the short-hand of an [XPath](https://en.wikipedia.org/wiki/XPath), except it also allows for arbitrary Lisp code to be executed. So, while it doesn't support the full RFC for XML queries, you should still be able to do a whole lot with it very easily.

Query strings can also be compiled and re-used with `xml-query-compile` instead of being re-compiled every use.

For example, let's load the RSS file in the test folder:

    CL-USER > (setf rss (xml-load #p"test/rss.xml"))
    #<XML::XML-DOC "rss">

Now, let's get the title of all RSS items:

    CL-USER > (xml-query rss "//item/title/%text")
    ("Star City"
     "The Engine That Does More"
     "Astronauts' Dirty Laundry")

The only difference between the above and an XPath was the use of `%text` instead of the XML query function `string()`. The `xml-query` system has a few dynamic variables that are always bound and can be referenced within the query:

**%node** The current node value (usually an `xml-node`, but not necessarily).

**%parent** When *%node* is an `xml-node`, this is the node's parent.

**%name** When *%node* is an `xml-node`, this is the node's name.

**%text** When *%node* is an `xml-node`, this is the node's value.

**%position** This is the index (1-based) of *%node* in the results.

In addition to the special variables, it's also possible to execute arbitrary Lisp code or call functions within the query. For example, let's parse all the links in the feed into URL objects.

    CL-USER > (xml-query rss "//link/(url:url-parse %text)")
    (http://liftoff.msfc.nasa.gov
     http://liftoff.msfc.nasa.gov/news/2003/news-starcity.asp
     http://liftoff.msfc.nasa.gov/news/2003/news-VASIMR.asp
     http://liftoff.msfc.nasa.gov/news/2003/news-laundry.asp)

Similarly, we can privide just a function that's used for a map and accomplish the same thing:

    CL-USER > (xml-query rss "//link/%text/'url:url-parse")
    (http://liftoff.msfc.nasa.gov
     http://liftoff.msfc.nasa.gov/news/2003/news-starcity.asp
     http://liftoff.msfc.nasa.gov/news/2003/news-VASIMR.asp
     http://liftoff.msfc.nasa.gov/news/2003/news-laundry.asp)

The queries also support sub-queries as filters. Let's find all items published on friday...

    CL-USER > (xml-query rss "//item/pubDate[(search \"Fri\" %text)]/..")
    (#<XML::XML-TAG "item">)

Or, we can just index directly to the item we want. Maybe the first?

    CL-USER > (xml-query rss "//item[1]")
    (#<XML::XML-TAG "item">)

*Note: Remember, in XPath queries all indices are 1-based!*

Indexing directly, however, is really just shorthand for filtering on the *%position*...

    CL-USER > (xml-query rss "/rss/*/item[(= %position 1)]")
    (#<XML::XML-TAG "item">)

Let's continue by first loading up the sample RDF feed...

    CL-USER > (setf rdf (xml-load "test/rdf.xml"))
    #<XML::XML-DOC "RDF">

Next, let's find all items that have an "about" tag.

    CL-USER > (xml-query rdf "//item[@about]")
    (#<XML::XML-TAG "item">
     #<XML::XML-TAG "item">
     #<XML::XML-TAG "item">)

Or, finally, how about all resource tags that are children of a "Seq" tag?

    CL-USER > (xml-query rdf "//@resource[../../../Seq]")
    (#<XML::XML-NODE "resource">
     #<XML::XML-NODE "resource">
     #<XML::XML-NODE "resource">)

As you can see, there's an aweful lot that's possible with the query language. Just to recap the basics:

**//** - Find descendants.

**/<*|tag>** - Find immediate child tags.

**/@attribute** - Find all child attributes.

**/(..)** - Map results through Lisp form.

**/'symbol** - Map results through Lisp symbol-function.

**[..]** - Filter results with a sub-query.

**[n]** - Filter by position (1-based).

**[(..)]** - Filter by Lisp form.

## Querying with Namespaces

Querying for tags and attributes can also use namespaces. You can filter by namespace as well as wildcarding ("\*"). Using the *rdf.xml* example in the test folder:

    CL-USER > (setf rdf (xml-load #p"test/rdf.xml"))
    #<XML-DOC "RDF">

    CL-USER > (xml-query rdf "//channel/*/Seq/rdf:li/@rdf:resource")
    (#<XML-ATTRIBUTE "resource">
     #<XML-ATTRIBUTE "resource">
     #<XML-ATTRIBUTE "resource">)

To quickly disect the example, it will...

1. ...start at the document root (since it's an absolute path),
2. ...recursively search for a 'channel' tag,
3. ...match all child tags,
4. ...search for any 'Seq' tags,
5. ...search for any 'li' tags in the 'rdf' namespace,
6. ...return 'resource' attributes in the 'rdf' namespace.

## Exported Methods

The `XML` package is pretty sparse by design.

There are a couple functions for parsing from a source file or string, and searching a document for tags using a path. All other methods exposed are accessors in the `xml-doc`, `xml-node`, or a few other classes.

#### Parsing and Loading XML Files

    (xml-parse string &optional source)     ;=> xml-doc
    (xml-load pathname)                     ;=> xml-doc
    (xml-read node)                         ;=> value

#### Traverse/Query Methods

    (xml-query [tag|doc] [query|string])    ;=> list
    (xml-query-compile string)              ;=> xml-query

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
    (xml-node-parent xml-node)              ;=> xml-node
    (xml-node-namespace xml-node)           ;=> xml-node
    (xml-node-name xml-node)                ;=> string
    (xml-node-value xml-node)               ;=> string

#### `xml-entity` methods (subclass of `xml-ref` and `xml-node`)

    (xml-entity-ndata xml-entity)           ;=> string

#### `xml-tag` methods (subclass of `xml-element`)

    (xml-tag-namespaces xml-tag)            ;=> xml-nodes
    (xml-tag-elements xml-tag)              ;=> xml-tags
    (xml-tag-attributes xml-tag)            ;=> xml-nodes

That's it!

## Feedback

Something not working right? Have a question or comment? Feel free to email me or create an issue on [GitHub](http://github.com/massung/xml).
