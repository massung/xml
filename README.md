# Introduction

A small, quick, and fairly-well featured XML parser for [LispWorks](http://www.lispworks.com) with a simple interface. It is only a parser (it is not intended for creating or writing XML files).

It makes heavy use of my [`re`](http://github.com/massung/re) and [`lexer`](http://github.com/massung/lexer) packages. So if you want to understand the code, start by understanding those first.

# License

This package uses the [Apache 2.0 license](http://www.apache.org/licenses/LICENSE-2.0). Please adhere to it.

# Quickstart

Once `xml.lisp` has been loaded you should be able to immediately begin parsing XML from both a string and a source file.

	CL-USER > (parse-xml "<!ENTITY who \"world\"><say>Hello, &who;!</say>")
	<XML::DOC say>

	CL-USER > (node-value (doc-root *))
	"Hello, world!"

	CL-USER > (parse-xml-file #p"test/rss.xml")
	<XML::DOC rss>

	CL-USER > (query-xml * "/rss/channel/item/title")
	(#<XML::TAG title>
	 #<XML::TAG title>
	 #<XML::TAG title>)

# Exported Methods

The `XML` package is pretty sparse by design. There are a couple functions for parsing from a source file or string, and searching a document for tags using an xpath. All other methods exposed are accessors in the `doc` or `tag` classes.

### Parsing Methods

	(parse-xml string)          ;=> doc
	(parse-xml-file pathname)   ;=> doc

### Traverse/Query Methods

	(query-xml tag xpath)       ;=> list
	(query-xml doc xpath)       ;=> list

	(query-attribute tag name)  ;=> attribute
	(query-attribute doc name)  ;=> attribute

### Document Accessors

	(doc-type doc)              ;=> list
	(doc-source doc)            ;=> pathname
	(doc-decl doc)              ;=> attributes
	(doc-entities doc)          ;=> list
	(doc-root doc)              ;=> tag

### Node Accessors

	(node-name node)            ;=> string
	(node-ns node)              ;=> string
	(node-doc node)             ;=> doc

#### Tag Accessors (subclass of `node`)

	(node-parent tag)           ;=> tag
	(node-attributes tag)       ;=> attributes
	(node-elements tag)         ;=> tags
	(node-value tag)            ;=> string

#### Attribute Accessors (subclass of `node`)

	(node-value attribute)   ;=> string

# How It Works

The XML package uses the [`lexer`](http://github.com/massung/lexer) package to tokenize the XML. It is actually six (6) separate lexers, which are used based on the current parse state: prolog, comments, tags, close tags, CDATA, and inner-xml lexers.

The parser reduces the tokens into a list of closures that - after being successfully parsed - will be called in order. This part can be thought of as if it were a SAX parser: pushing tags, writing inner text, popping tags, etc. These functions make use of the following special variables:

* `*XML-DOC*` is the top-level document.
* `*XML-ROOT*` is the root tag.
* `*XML-STACK*` is the stack of tags being created.
* `*XML-TAG*` is the current tag being created.

Once parsed, a new `*XML-DOC*` is created and the first set of closures executed will set the `doc-decl` attributes, `doc-type` DOCTYPE, and `doc-entities` ENTITIES.

When a new tag is found, the current `*xml-tag*` is pushed onto the `*xml-stack*` and a new tag is created with the current `*xml-tag*` as its parent. The `inner-text` property of the new tag is a string output stream. As text is found, it is written to the stream.

As tags are closed, they are first validated and then pushed onto the `node-elements` property of their parent tag. The `inner-text` stream is then flushed for its text and `*xml-root*` is set to `*xml-tag*`. Finally, `*xml-stack*` is popped into `*xml-tag*` and generation of the XML document tree continues.

This continues until there are no more closures to execute, at which time `*xml-doc*` is complete and `*xml-root*` should be the top-level node of the document. 

# Feedback

Something not working right? Have a question or comment? Feel free to email me or create an issue on [GitHub](http://github.com/massung/xml).
