# Introduction

A small, quick, and fairly-well featured XML parser for [LispWorks](http://www.lispworks.com) with a simple interface. It is only a parser (it is not intended for creating or writing XML files).

It makes heavy use of my [`re`](http://github.com/massung/re) and [`lexer`](http://github.com/massung/lexer) packages. So if you want to understand the code, start by understanding those first.

## Quickstart

Once `xml.lisp` has been loaded you should be able to immediately begin parsing XML from both a string and a source file.

	CL-USER > (xml-parse "<test>Hello, world!</test>")
	#<XML::XML-DOC "test">

	CL-USER > (xml-node-value (xml-doc-root *))
	"Hello, world!"
	
In the test folder are a couple files you can try parsing as well.

	CL-USER > (xml-load #p"test/test.xml")
	#<XML::XML-DOC "test">
	
The test file is *extremely* featured and is used to test everything that the XML library does.

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

#### Loading and Parsing XML Files

	(xml-load pathname &key source-encoding)         ;=> xml-doc

#### Parsing XML Files

	(xml-parse string &key source source-encoding)   ;=> xml-doc
	
#### Parsing XML Inner Text and Attribute Lists
	
	(xml-parse-text string &key document)            ;=> string
	(xml-parse-attributes string &key document)      ;=> xml-attributes

#### Traverse/Query Methods

	(xml-query xml-tag path &key all)                ;=> xml-tags
	(xml-query xml-doc path &key all)                ;=> xml-tags

	(xml-query-attribute xml-tag name)               ;=> xml-attribute
	(xml-query-attribute xml-doc name)               ;=> xml-attribute

#### `xml-doc` methods

	(xml-doc-root xml-doc)                           ;=> xml-tag
	(xml-doc-doctype xml-doc)                        ;=> xml-doctype
	(xml-doc-entities xml-doc)                       ;=> xml-entities
	(xml-doc-instructions xml-doc)                   ;=> xml-processing-instructions
	(xml-doc-source xml-doc)                         ;=> pathname
	(xml-doc-source-encoding xml-doc)                ;=> keyword
	(xml-doc-encoding xml-doc)                       ;=> keyword
	(xml-doc-standalone xml-doc)                     ;=> boolean
	(xml-doc-version xml-doc)                        ;=> string
	
#### `xml-external-ref` methods

	(xml-external-ref-public-id xml-external-ref)    ;=> string
	(xml-external-ref-system-uri xml-external-ref)   ;=> string
	
#### `xml-doctype` methods (subclass of `xml-external-ref`)

	(xml-doctype-root xml-doctype)                   ;=> string

#### `xml-node` methods

	(xml-node-name xml-node)                         ;=> string
	(xml-node-value xml-node)                        ;=> string
	(xml-node-doc xml-node)                          ;=> xml-doc

#### `xml-tag` methods (subclass of `xml-node`)

	(xml-tag-attributes xml-tag)                     ;=> xml-attributes
	(xml-tag-elements xml-tag)                       ;=> xml-tags
	(xml-tag-instructions xml-tag)                   ;=> xml-processing-instructions
	
Processing instructions and attributes are simple subclasses of `xml-node`. The entity class is both a subclass of `xml-node` and `xml-external-ref`.

That's it!

## Feedback

Something not working right? Have a question or comment? Feel free to email me or create an issue on [GitHub](http://github.com/massung/xml).
