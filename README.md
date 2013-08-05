# Introduction

A small, quick, and fairly-well featured XML parser for [LispWorks](!http://www.lispworks.com) with a simple interface. It is only a parser (it is not intended for creating or writing XML files).

# License

This package uses the [Apache 2.0 license](!http://www.apache.org/licenses/LICENSE-2.0). Please adhere to it.

# Quickstart

Once `xml.lisp` has been loaded (it is recommended to use [`defsystem`](!http://www.lispworks.com/documentation/lw445/LWRM/html/lwref-273.htm) to load it), you should be able to immediately begin parsing XML from both a string and a source file:

	CL-USER > (use-package :xml)
	T

	CL-USER > (parse-xml "<!entity who \"world\"><say>hello, &who;</say>")
	<XML::TAG say>

	CL-USER > (tag-text *)
	"hello, world"

	CL-USER > (parse-xml-file #p"examples/rss.xml")
	<XML::TAG rss>

	CL-USER > (query-xml * "/channel/item/title")
	(#<XML::TAG title> #<XML::TAG title> #<XML::TAG title>)

# Exported Methods

### Parsing Methods
	(parse-xml string)        ;=> tag
	(parse-xml-file pathname) ;=> tag

### Traverse/Query Methods

	(query-xml tag xpath)     ;=> list

### Document Methods

	(doc-type doc)            ;=> list
	(doc-source doc)          ;=> pathname
	(doc-decl doc)            ;=> attributes
	(doc-entities doc)        ;=> list
	(doc-root doc)            ;=> tag

### Tag Methods

	(tag-doc tag)             ;=> doc
	(tag-name tag)            ;=> string
	(tag-ns tag)              ;=> string
	(tag-parent tag)          ;=> tag
	(tag-attributes tag)      ;=> list
	(tag-elements tag)        ;=> list
	(tag-text tag)            ;=> string

# How It Works

The XML package uses the [DEFLEXER](!http://github.com/massung/lexer) package to tokenize the XML. It is actually four (4) separate lexers, which are used based on the current parse state: comments, tags, CDATA, and body lexers.

As the tokens are parsed, the lexer may end up switching the `*XML-LEXER*` global variable to change which tokenizer is being used. This is how the parser can differentiate between the tag `<foo>` and the inner text `"foo"` within the tag.

The parser reduces the tokens into a list of closures that - after being successfully parsed - will be called in order. This part can be thought of as if it were a SAX parser: pushing tags, writing inner text, popping tags, etc. These functions make use of the following special variables:

* `*XML-LEXER*` is the current lexical state function.
* `*XML-DOC*` is the top-level document.
* `*XML-ROOT*` is the root tag.
* `*XML-TAG*` is the current tag being parsed.
* `*XML-STACK*` is the stack of tags being parsed.

While being built, each tag's `inner-text` member is actually an output stream that is written to. When each tag is closed, the `inner-text` member is flushed and the output string is set.

# Feedback

Something not working right? Have a question or comment? Feel free to email me or create an issue on [GitHub](!http://github.com/massung/xml).
