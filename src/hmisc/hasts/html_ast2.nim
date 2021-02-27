import options, macros, strformat, strutils
import hmisc/macros/matching
{.experimental: "caseStmtMacros".}

# Copied from
# https://developer.mozilla.org/en-US/docs/Web/HTML/Element and
# converted using:
when false:
  import strutils, strformat
  import std/wordwrap

  let ind = "    "

  for line in lines("/tmp/txt"):
    let spl = line.split(" ")
    if spl[0].startsWith("<"):
      let str = spl[0][1..^2]
      let pref = (
        &"html{str.capitalizeAscii()} = \"{str}\""
      ).alignLeft(30)
      let mult = wrapWords(spl.join(" "), 60).split("\n")
      echo ind, pref, " ## |"
      for line in mult:
        echo ind, "## ", line
    else:
      echo ind, "# ", line

type
  HtmlNodeKind* = enum
    # Document metadata
    #
    # Metadata contains information about the page. This includes information about styles, scripts and data to help software (search engines, browsers, etc.) use and render the page. Metadata for styles and scripts may be defined in the page or link to another file that has the information.
    # Element 	Description
    htmlBase = "base"              ## |
    ## <base> 	The HTML <base> element specifies the base URL to
    ## use for all relative URLs in a document.
    htmlHead = "head"              ## |
    ## <head> 	The HTML <head> element contains machine-readable
    ## information (metadata) about the document, like its title,
    ## scripts, and style sheets.
    htmlLink = "link"              ## |
    ## <link> 	The HTML External Resource Link element (<link>)
    ## specifies relationships between the current document and an
    ## external resource. This element is most commonly used to
    ## link to stylesheets, but is also used to establish site
    ## icons (both "favicon" style icons and icons for the home
    ## screen and apps on mobile devices) among other things.
    htmlMeta = "meta"              ## |
    ## <meta> 	The HTML <meta> element represents metadata that
    ## cannot be represented by other HTML meta-related elements,
    ## like <base>, <link>, <script>, <style> or <title>.
    htmlStyle = "style"            ## |
    ## <style> 	The HTML <style> element contains style information
    ## for a document, or part of a document.
    htmlTitle = "title"            ## |
    ## <title> 	The HTML Title element (<title>) defines the
    ## document's title that is shown in a browser's title bar or a
    ## page's tab.
    # Sectioning root
    # Element 	Description
    htmlBody = "body"              ## |
    ## <body> 	The HTML <body> Element represents the content of an
    ## HTML document. There can be only one <body> element in a
    ## document.
    # Content sectioning
    #
    # Content sectioning elements allow you to organize the document content into logical pieces. Use the sectioning elements to create a broad outline for your page content, including header and footer navigation, and heading elements to identify sections of content.
    # Element 	Description
    htmlAddress = "address"        ## |
    ## <address> 	The HTML <address> element indicates that the
    ## enclosed HTML provides contact information for a person or
    ## people, or for an organization.
    htmlArticle = "article"        ## |
    ## <article> 	The HTML <article> element represents a
    ## self-contained composition in a document, page, application,
    ## or site, which is intended to be independently distributable
    ## or reusable (e.g., in syndication).
    htmlAside = "aside"            ## |
    ## <aside> 	The HTML <aside> element represents a portion of a
    ## document whose content is only indirectly related to the
    ## document's main content.
    htmlFooter = "footer"          ## |
    ## <footer> 	The HTML <footer> element represents a footer for
    ## its nearest sectioning content or sectioning root element. A
    ## footer typically contains information about the author of
    ## the section, copyright data or links to related documents.
    htmlHeader = "header"          ## |
    ## <header> 	The HTML <header> element represents introductory
    ## content, typically a group of introductory or navigational
    ## aids. It may contain some heading elements but also a logo,
    ## a search form, an author name, and other elements.
    htmlH1 = "h1"                ## |
    htmlH2 = "h2"                ## |
    htmlH3 = "h3"                ## |
    htmlH4 = "h4"                ## |
    htmlH5 = "h5"                ## |
    htmlH6 = "h6"                ## |
    ## <h1>, <h2>, <h3>, <h4>, <h5>, <h6> 	The HTML <h1>–<h6>
    ## elements represent six levels of section headings. <h1> is
    ## the highest section level and <h6> is the lowest.
    htmlHgroup = "hgroup"          ## |
    ## <hgroup> 	The HTML <hgroup> element represents a multi-level
    ## heading for a section of a document. It groups a set of
    ## <h1>–<h6> elements.
    htmlMain = "main"              ## |
    ## <main> 	The HTML <main> element represents the dominant
    ## content of the <body> of a document. The main content area
    ## consists of content that is directly related to or expands
    ## upon the central topic of a document, or the central
    ## functionality of an application.
    htmlNav = "nav"                ## |
    ## <nav> 	The HTML <nav> element represents a section of a page
    ## whose purpose is to provide navigation links, either within
    ## the current document or to other documents. Common examples
    ## of navigation sections are menus, tables of contents, and
    ## indexes.
    htmlSection = "section"        ## |
    ## <section> 	The HTML <section> element represents a
    ## standalone section — which doesn't have a more specific
    ## semantic element to represent it — contained within an HTML
    ## document.
    # Text content
    #
    # Use HTML text content elements to organize blocks or sections of content placed between the opening <body> and closing </body> tags. Important for accessibility and SEO, these elements identify the purpose or structure of that content.
    # Element 	Description
    htmlBlockquote = "blockquote"  ## |
    ## <blockquote> 	The HTML <blockquote> Element (or HTML Block
    ## Quotation Element) indicates that the enclosed text is an
    ## extended quotation. Usually, this is rendered visually by
    ## indentation (see Notes for how to change it). A URL for the
    ## source of the quotation may be given using the cite
    ## attribute, while a text representation of the source can be
    ## given using the <cite> element.
    htmlDd = "dd"                  ## |
    ## <dd> 	The HTML <dd> element provides the description,
    ## definition, or value for the preceding term (<dt>) in a
    ## description list (<dl>).
    htmlDiv = "div"                ## |
    ## <div> 	The HTML Content Division element (<div>) is the
    ## generic container for flow content. It has no effect on the
    ## content or layout until styled using CSS.
    htmlDl = "dl"                  ## |
    ## <dl> 	The HTML <dl> element represents a description list.
    ## The element encloses a list of groups of terms (specified
    ## using the <dt> element) and descriptions (provided by <dd>
    ## elements). Common uses for this element are to implement a
    ## glossary or to display metadata (a list of key-value pairs).
    htmlDt = "dt"                  ## |
    ## <dt> 	The HTML <dt> element specifies a term in a
    ## description or definition list, and as such must be used
    ## inside a <dl> element.
    htmlFigcaption = "figcaption"  ## |
    ## <figcaption> 	The HTML <figcaption> or Figure Caption
    ## element represents a caption or legend describing the rest
    ## of the contents of its parent <figure> element.
    htmlFigure = "figure"          ## |
    ## <figure> 	The HTML <figure> (Figure With Optional Caption)
    ## element represents self-contained content, potentially with
    ## an optional caption, which is specified using the
    ## (<figcaption>) element.
    htmlHr = "hr"                  ## |
    ## <hr> 	The HTML <hr> element represents a thematic break
    ## between paragraph-level elements: for example, a change of
    ## scene in a story, or a shift of topic within a section.
    htmlLi = "li"                  ## |
    ## <li> 	The HTML <li> element is used to represent an item in
    ## a list.
    htmlOl = "ol"                  ## |
    ## <ol> 	The HTML <ol> element represents an ordered list of
    ## items — typically rendered as a numbered list.
    htmlP = "p"                    ## |
    ## <p> 	The HTML <p> element represents a paragraph.
    htmlPre = "pre"                ## |
    ## <pre> 	The HTML <pre> element represents preformatted text
    ## which is to be presented exactly as written in the HTML
    ## file.
    htmlUl = "ul"                  ## |
    ## <ul> 	The HTML <ul> element represents an unordered list of
    ## items, typically rendered as a bulleted list.
    # Inline text semantics
    #
    # Use the HTML inline text semantic to define the meaning, structure, or style of a word, line, or any arbitrary piece of text.
    # Element 	Description
    htmlA = "a"                    ## |
    ## <a> 	The HTML <a> element (or anchor element), with its href
    ## attribute, creates a hyperlink to web pages, files, email
    ## addresses, locations in the same page, or anything else a
    ## URL can address.
    htmlAbbr = "abbr"              ## |
    ## <abbr> 	The HTML Abbreviation element (<abbr>) represents an
    ## abbreviation or acronym; the optional title attribute can
    ## provide an expansion or description for the abbreviation.
    htmlB = "b"                    ## |
    ## <b> 	The HTML Bring Attention To element (<b>) is used to
    ## draw the reader's attention to the element's contents, which
    ## are not otherwise granted special importance.
    htmlBdi = "bdi"                ## |
    ## <bdi> 	The HTML Bidirectional Isolate element (<bdi>)  tells
    ## the browser's bidirectional algorithm to treat the text it
    ## contains in isolation from its surrounding text.
    htmlBdo = "bdo"                ## |
    ## <bdo> 	The HTML Bidirectional Text Override element (<bdo>)
    ## overrides the current directionality of text, so that the
    ## text within is rendered in a different direction.
    htmlBr = "br"                  ## |
    ## <br> 	The HTML <br> element produces a line break in text
    ## (carriage-return). It is useful for writing a poem or an
    ## address, where the division of lines is significant.
    htmlCite = "cite"              ## |
    ## <cite> 	The HTML Citation element (<cite>) is used to
    ## describe a reference to a cited creative work, and must
    ## include the title of that work.
    htmlCode = "code"              ## |
    ## <code> 	The HTML <code> element displays its contents styled
    ## in a fashion intended to indicate that the text is a short
    ## fragment of computer code.
    htmlData = "data"              ## |
    ## <data> 	The HTML <data> element links a given piece of
    ## content with a machine-readable translation. If the content
    ## is time- or date-related, the <time> element must be used.
    htmlDfn = "dfn"                ## |
    ## <dfn> 	The HTML Definition element (<dfn>) is used to
    ## indicate the term being defined within the context of a
    ## definition phrase or sentence.
    htmlEm = "em"                  ## |
    ## <em> 	The HTML <em> element marks text that has stress
    ## emphasis. The <em> element can be nested, with each level of
    ## nesting indicating a greater degree of emphasis.
    htmlI = "i"                    ## |
    ## <i> 	The HTML Idiomatic Text element (<i>) represents a
    ## range of text that is set off from the normal text for some
    ## reason, such as idiomatic text, technical terms, taxonomical
    ## designations, among others.
    htmlKbd = "kbd"                ## |
    ## <kbd> 	The HTML Keyboard Input element (<kbd>) represents a
    ## span of inline text denoting textual user input from a
    ## keyboard, voice input, or any other text entry device.
    htmlMark = "mark"              ## |
    ## <mark> 	The HTML Mark Text element (<mark>) represents text
    ## which is marked or highlighted for reference or notation
    ## purposes, due to the marked passage's relevance or
    ## importance in the enclosing context.
    htmlQ = "q"                    ## |
    ## <q> 	The HTML <q> element indicates that the enclosed text
    ## is a short inline quotation. Most modern browsers implement
    ## this by surrounding the text in quotation marks.
    htmlRb = "rb"                  ## |
    ## <rb> 	The HTML Ruby Base (<rb>) element is used to delimit
    ## the base text component of a  <ruby> annotation, i.e. the
    ## text that is being annotated.
    htmlRp = "rp"                  ## |
    ## <rp> 	The HTML Ruby Fallback Parenthesis (<rp>) element is
    ## used to provide fall-back parentheses for browsers that do
    ## not support display of ruby annotations using the <ruby>
    ## element.
    htmlRt = "rt"                  ## |
    ## <rt> 	The HTML Ruby Text (<rt>) element specifies the ruby
    ## text component of a ruby annotation, which is used to
    ## provide pronunciation, translation, or transliteration
    ## information for East Asian typography. The <rt> element must
    ## always be contained within a <ruby> element.
    htmlRtc = "rtc"                ## |
    ## <rtc> 	The HTML Ruby Text Container (<rtc>) element embraces
    ## semantic annotations of characters presented in a ruby of
    ## <rb> elements used inside of <ruby> element. <rb> elements
    ## can have both pronunciation (<rt>) and semantic (<rtc>)
    ## annotations.
    htmlRuby = "ruby"              ## |
    ## <ruby> 	The HTML <ruby> element represents small annotations
    ## that are rendered above, below, or next to base text,
    ## usually used for showing the pronunciation of East Asian
    ## characters. It can also be used for annotating other kinds
    ## of text, but this usage is less common.
    htmlS = "s"                    ## |
    ## <s> 	The HTML <s> element renders text with a strikethrough,
    ## or a line through it. Use the <s> element to represent
    ## things that are no longer relevant or no longer accurate.
    ## However, <s> is not appropriate when indicating document
    ## edits; for that, use the <del> and <ins> elements, as
    ## appropriate.
    htmlSamp = "samp"              ## |
    ## <samp> 	The HTML Sample Element (<samp>) is used to enclose
    ## inline text which represents sample (or quoted) output from
    ## a computer program.
    htmlSmall = "small"            ## |
    ## <small> 	The HTML <small> element represents side-comments
    ## and small print, like copyright and legal text, independent
    ## of its styled presentation. By default, it renders text
    ## within it one font-size smaller, such as from small to
    ## x-small.
    htmlSpan = "span"              ## |
    ## <span> 	The HTML <span> element is a generic inline
    ## container for phrasing content, which does not inherently
    ## represent anything. It can be used to group elements for
    ## styling purposes (using the class or id attributes), or
    ## because they share attribute values, such as lang.
    htmlStrong = "strong"          ## |
    ## <strong> 	The HTML Strong Importance Element (<strong>)
    ## indicates that its contents have strong importance,
    ## seriousness, or urgency. Browsers typically render the
    ## contents in bold type.
    htmlSub = "sub"                ## |
    ## <sub> 	The HTML Subscript element (<sub>) specifies inline
    ## text which should be displayed as subscript for solely
    ## typographical reasons.
    htmlSup = "sup"                ## |
    ## <sup> 	The HTML Superscript element (<sup>) specifies inline
    ## text which is to be displayed as superscript for solely
    ## typographical reasons.
    htmlTime = "time"              ## |
    ## <time> 	The HTML <time> element represents a specific period
    ## in time.
    htmlU = "u"                    ## |
    ## <u> 	The HTML Unarticulated Annotation element (<u>)
    ## represents a span of inline text which should be rendered in
    ## a way that indicates that it has a non-textual annotation.
    htmlVar = "var"                ## |
    ## <var> 	The HTML Variable element (<var>) represents the name
    ## of a variable in a mathematical expression or a programming
    ## context.
    htmlWbr = "wbr"                ## |
    ## <wbr> 	The HTML <wbr> element represents a word break
    ## opportunity—a position within text where the browser may
    ## optionally break a line, though its line-breaking rules
    ## would not otherwise create a break at that location.
    # Image and multimedia
    #
    # HTML supports various multimedia resources such as images, audio, and video.
    # Element 	Description
    htmlArea = "area"              ## |
    ## <area> 	The HTML <area> tag defines an area inside an image
    ## map that has predefined clickable areas. An image map allows
    ## geometric areas on an image to be associated with hypertext
    ## link.
    htmlAudio = "audio"            ## |
    ## <audio> 	The HTML <audio> element is used to embed sound
    ## content in documents. It may contain one or more audio
    ## sources, represented using the src attribute or the <source>
    ## element: the browser will choose the most suitable one. It
    ## can also be the destination for streamed media, using a
    ## MediaStream.
    htmlImg = "img"                ## |
    ## <img> 	The HTML <img> element embeds an image into the
    ## document.
    htmlMap = "map"                ## |
    ## <map> 	The HTML <map> element is used with <area> elements
    ## to define an image map (a clickable link area).
    htmlTrack = "track"            ## |
    ## <track> 	The HTML <track> element is used as a child of the
    ## media elements, <audio> and <video>. It lets you specify
    ## timed text tracks (or time-based data), for example to
    ## automatically handle subtitles.
    htmlVideo = "video"            ## |
    ## <video> 	The HTML Video element (<video>) embeds a media
    ## player which supports video playback into the document. You
    ## can use <video> for audio content as well, but the <audio>
    ## element may provide a more appropriate user experience.
    # Embedded content
    #
    # In addition to regular multimedia content, HTML can include a variety of other content, even if it's not always easy to interact with.
    # Element 	Description
    htmlEmbed = "embed"            ## |
    ## <embed> 	The HTML <embed> element embeds external content at
    ## the specified point in the document. This content is
    ## provided by an external application or other source of
    ## interactive content such as a browser plug-in.
    htmlIframe = "iframe"          ## |
    ## <iframe> 	The HTML Inline Frame element (<iframe>)
    ## represents a nested browsing context, embedding another HTML
    ## page into the current one.
    htmlObject = "object"          ## |
    ## <object> 	The HTML <object> element represents an external
    ## resource, which can be treated as an image, a nested
    ## browsing context, or a resource to be handled by a plugin.
    htmlParam = "param"            ## |
    ## <param> 	The HTML <param> element defines parameters for an
    ## <object> element.
    htmlPicture = "picture"        ## |
    ## <picture> 	The HTML <picture> element contains zero or more
    ## <source> elements and one <img> element to offer alternative
    ## versions of an image for different display/device scenarios.
    htmlSource = "source"          ## |
    ## <source> 	The HTML <source> element specifies multiple media
    ## resources for the <picture>, the <audio> element, or the
    ## <video> element.
    # Scripting
    #
    # In order to create dynamic content and Web applications, HTML supports the use of scripting languages, most prominently JavaScript. Certain elements support this capability.
    # Element 	Description
    htmlCanvas = "canvas"          ## |
    ## <canvas> 	Use the HTML <canvas> element with either the
    ## canvas scripting API or the WebGL API to draw graphics and
    ## animations.
    htmlNoscript = "noscript"      ## |
    ## <noscript> 	The HTML <noscript> element defines a section of
    ## HTML to be inserted if a script type on the page is
    ## unsupported or if scripting is currently turned off in the
    ## browser.
    htmlScript = "script"          ## |
    ## <script> 	The HTML <script> element is used to embed
    ## executable code or data; this is typically used to embed or
    ## refer to JavaScript code.
    # Demarcating edits
    #
    # These elements let you provide indications that specific parts of the text have been altered.
    # Element 	Description
    htmlDel = "del"                ## |
    ## <del> 	The HTML <del> element represents a range of text
    ## that has been deleted from a document.
    htmlIns = "ins"                ## |
    ## <ins> 	The HTML <ins> element represents a range of text
    ## that has been added to a document.
    # Table content
    #
    # The elements here are used to create and handle tabular data.
    # Element 	Description
    htmlCaption = "caption"        ## |
    ## <caption> 	The HTML <caption> element specifies the caption
    ## (or title) of a table.
    htmlCol = "col"                ## |
    ## <col> 	The HTML <col> element defines a column within a
    ## table and is used for defining common semantics on all
    ## common cells. It is generally found within a <colgroup>
    ## element.
    htmlColgroup = "colgroup"      ## |
    ## <colgroup> 	The HTML <colgroup> element defines a group of
    ## columns within a table.
    htmlTable = "table"            ## |
    ## <table> 	The HTML <table> element represents tabular data —
    ## that is, information presented in a two-dimensional table
    ## comprised of rows and columns of cells containing data.
    htmlTbody = "tbody"            ## |
    ## <tbody> 	The HTML Table Body element (<tbody>) encapsulates
    ## a set of table rows (<tr> elements), indicating that they
    ## comprise the body of the table (<table>).
    htmlTd = "td"                  ## |
    ## <td> 	The HTML <td> element defines a cell of a table that
    ## contains data. It participates in the table model.
    htmlTfoot = "tfoot"            ## |
    ## <tfoot> 	The HTML <tfoot> element defines a set of rows
    ## summarizing the columns of the table.
    htmlTh = "th"                  ## |
    ## <th> 	The HTML <th> element defines a cell as header of a
    ## group of table cells. The exact nature of this group is
    ## defined by the scope and headers attributes.
    htmlThead = "thead"            ## |
    ## <thead> 	The HTML <thead> element defines a set of rows
    ## defining the head of the columns of the table.
    htmlTr = "tr"                  ## |
    ## <tr> 	The HTML <tr> element defines a row of cells in a
    ## table. The row's cells can then be established using a mix
    ## of <td> (data cell) and <th> (header cell) elements.
    # Forms
    #
    # HTML provides a number of elements which can be used together to create forms which the user can fill out and submit to the Web site or application. There's a great deal of further information about this available in the HTML forms guide.
    # Element 	Description
    htmlButton = "button"          ## |
    ## <button> 	The HTML <button> element represents a clickable
    ## button, used to submit forms or anywhere in a document for
    ## accessible, standard button functionality.
    htmlDatalist = "datalist"      ## |
    ## <datalist> 	The HTML <datalist> element contains a set of
    ## <option> elements that represent the permissible or
    ## recommended options available to choose from within other
    ## controls.
    htmlFieldset = "fieldset"      ## |
    ## <fieldset> 	The HTML <fieldset> element is used to group
    ## several controls as well as labels (<label>) within a web
    ## form.
    htmlForm = "form"              ## |
    ## <form> 	The HTML <form> element represents a document
    ## section containing interactive controls for submitting
    ## information.
    htmlInput = "input"            ## |
    ## <input> 	The HTML <input> element is used to create
    ## interactive controls for web-based forms in order to accept
    ## data from the user; a wide variety of types of input data
    ## and control widgets are available, depending on the device
    ## and user agent.
    htmlLabel = "label"            ## |
    ## <label> 	The HTML <label> element represents a caption for
    ## an item in a user interface.
    htmlLegend = "legend"          ## |
    ## <legend> 	The HTML <legend> element represents a caption for
    ## the content of its parent <fieldset>.
    htmlMeter = "meter"            ## |
    ## <meter> 	The HTML <meter> element represents either a scalar
    ## value within a known range or a fractional value.
    htmlOptgroup = "optgroup"      ## |
    ## <optgroup> 	The HTML <optgroup> element creates a grouping
    ## of options within a <select> element.
    htmlOption = "option"          ## |
    ## <option> 	The HTML <option> element is used to define an
    ## item contained in a <select>, an <optgroup>, or a <datalist>
    ## element. As such, <option> can represent menu items in
    ## popups and other lists of items in an HTML document.
    htmlOutput = "output"          ## |
    ## <output> 	The HTML Output element (<output>) is a container
    ## element into which a site or app can inject the results of a
    ## calculation or the outcome of a user action.
    htmlProgress = "progress"      ## |
    ## <progress> 	The HTML <progress> element displays an
    ## indicator showing the completion progress of a task,
    ## typically displayed as a progress bar.
    htmlSelect = "select"          ## |
    ## <select> 	The HTML <select> element represents a control
    ## that provides a menu of options
    htmlTextarea = "textarea"      ## |
    ## <textarea> 	The HTML <textarea> element represents a
    ## multi-line plain-text editing control, useful when you want
    ## to allow users to enter a sizeable amount of free-form text,
    ## for example a comment on a review or feedback form.
    # Interactive elements
    #
    # HTML offers a selection of elements which help to create interactive user interface objects.
    # Element 	Description
    htmlDetails = "details"        ## |
    ## <details> 	The HTML Details Element (<details>) creates a
    ## disclosure widget in which information is visible only when
    ## the widget is toggled into an "open" state.
    htmlDialog = "dialog"          ## |
    ## <dialog> 	The HTML <dialog> element represents a dialog box
    ## or other interactive component, such as a dismissable alert,
    ## inspector, or subwindow.
    htmlMenu = "menu"              ## |
    ## <menu> 	The HTML <menu> element represents a group of
    ## commands that a user can perform or activate. This includes
    ## both list menus, which might appear across the top of a
    ## screen, as well as context menus, such as those that might
    ## appear underneath a button after it has been clicked.
    htmlSummary = "summary"        ## |
    ## <summary> 	The HTML Disclosure Summary element (<summary>)
    ## element specifies a summary, caption, or legend for a
    ## <details> element's disclosure box.
    # Web Components
    #
    # Web Components is an HTML-related technology which makes it possible to, essentially, create and use custom elements as if it were regular HTML. In addition, you can create custom versions of standard HTML elements.
    # Element 	Description
    htmlSlot = "slot"              ## |
    ## <slot> 	The HTML <slot> element—part of the Web Components
    ## technology suite—is a placeholder inside a web component
    ## that you can fill with your own markup, which lets you
    ## create separate DOM trees and present them together.
    htmlTemplate = "template"      ## |
    ## <template> 	The HTML Content Template (<template>) element
    ## is a mechanism for holding HTML that is not to be rendered
    ## immediately when a page is loaded but may be instantiated
    ## subsequently during runtime using JavaScript.

  HtmlNode* = ref object
    kind*: HtmlNodeKind
    text*: string
    subn*: seq[HtmlNode]

func add*(node: var HtmlNode, subn: HtmlNode) = node.subn.add subn
func add*(node: var HtmlNode, text: string) = node.text.add text

func render*(inNode: HtmlNode): string =
  func aux(node: HtmlNode, level: int): seq[string] =
    let pref = "  ".repeat(level)
    if node.subn.len == 0:
      result.add &"{pref}<{node.kind}>{node.text}</{node.kind}>"
    else:
      result.add &"{pref}<{node.kind}>"
      for subn in node.subn:
        result.add aux(subn, level + 1)
      result.add &"{pref}</{node.kind}>"

  return aux(inNode, 0).join("\n")

template newHtml(body: untyped): untyped =
  makeTree(HtmlNode):
    body

let tree = newHtml:
  h1:
    h1: b: "text"
    h2:
      b(text: "Nice")

echo tree.render
