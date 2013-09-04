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

	(doc-source doc)            ;=> pathname
	(doc-decl doc)              ;=> attributes
	(doc-type doc)              ;=> list
	(doc-prolog doc)            ;=> prolog
	(doc-root doc)              ;=> tag

### Prolog Accessors

	(prolog-stylesheets prolog) ;=> list
	(prolog-entities prolog)    ;=> list

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

	(node-value attribute)      ;=> string

# How It Works

The XML package uses the [`lexer`](http://github.com/massung/lexer) package to tokenize the XML. After tokenizing and parsing, there is an XML "form" that can be evaluated to build the document that looks something like this:

	(decl doctype prolog root)

The declaration is a list of attributes parsed from the `<?xml?>` declaration. The doctype is what was parsed in the `<!DOCTYPE>` tag. The prolog consists of all the `<?xml-stylesheet?>` and `<!ENTITY>` tags.

The root where where all the interesting forms are. Each tag is in a list that resembles:

	((name &optional ns) attributes inner-forms)

The name, namespace, and attributes are fairly simple. The inner-forms are child elements that can be one of the following:

	(:tag tag-form)
	(:text string)
	(:cdata string)
	(:close-tag (name &optional ns))

The tag is constructed and then all inner-forms are iterated over and built.

# Feedback

Something not working right? Have a question or comment? Feel free to email me or create an issue on [GitHub](http://github.com/massung/xml).
