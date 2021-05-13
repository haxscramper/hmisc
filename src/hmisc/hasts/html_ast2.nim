import std/[options, macros, strformat, strutils, streams]
import std/[macros, sequtils]

import ../other/oswrap
import ../algo/halgorithm

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
    # Metadata contains information about the page. This includes
    # information about styles, scripts and data to help software (search
    # engines, browsers, etc.) use and render the page. Metadata for styles
    # and scripts may be defined in the page or link to another file that
    # has the information. Element Description
    hHtml = "html"
    hBase = "base"              ## |
    ## <base>  The HTML <base> element specifies the base URL to
    ## use for all relative URLs in a document.
    hHead = "head"              ## |
    ## <head>  The HTML <head> element contains machine-readable
    ## information (metadata) about the document, like its title,
    ## scripts, and style sheets.
    hLink = "link"              ## |
    ## <link>  The HTML External Resource Link element (<link>)
    ## specifies relationships between the current document and an
    ## external resource. This element is most commonly used to
    ## link to stylesheets, but is also used to establish site
    ## icons (both "favicon" style icons and icons for the home
    ## screen and apps on mobile devices) among other things.
    hMeta = "meta"              ## |
    ## <meta>  The HTML <meta> element represents metadata that
    ## cannot be represented by other HTML meta-related elements,
    ## like <base>, <link>, <script>, <style> or <title>.
    hStyle = "style"            ## |
    ## <style>  The HTML <style> element contains style information
    ## for a document, or part of a document.
    hTitle = "title"            ## |
    ## <title>  The HTML Title element (<title>) defines the
    ## document's title that is shown in a browser's title bar or a
    ## page's tab.
    # Sectioning root
    # Element  Description
    hBody = "body"              ## |
    ## <body>  The HTML <body> Element represents the content of an
    ## HTML document. There can be only one <body> element in a
    ## document.
    # Content sectioning
    #
    # Content sectioning elements allow you to organize the document content into logical pieces. Use the sectioning elements to create a broad outline for your page content, including header and footer navigation, and heading elements to identify sections of content.
    # Element  Description
    hAddress = "address"        ## |
    ## <address>  The HTML <address> element indicates that the
    ## enclosed HTML provides contact information for a person or
    ## people, or for an organization.
    hArticle = "article"        ## |
    ## <article>  The HTML <article> element represents a
    ## self-contained composition in a document, page, application,
    ## or site, which is intended to be independently distributable
    ## or reusable (e.g., in syndication).
    hAside = "aside"            ## |
    ## <aside>  The HTML <aside> element represents a portion of a
    ## document whose content is only indirectly related to the
    ## document's main content.
    hFooter = "footer"          ## |
    ## <footer>  The HTML <footer> element represents a footer for
    ## its nearest sectioning content or sectioning root element. A
    ## footer typically contains information about the author of
    ## the section, copyright data or links to related documents.
    hHeader = "header"          ## |
    ## <header>  The HTML <header> element represents introductory
    ## content, typically a group of introductory or navigational
    ## aids. It may contain some heading elements but also a logo,
    ## a search form, an author name, and other elements.
    hH1 = "h1"                ## |
    hH2 = "h2"                ## |
    hH3 = "h3"                ## |
    hH4 = "h4"                ## |
    hH5 = "h5"                ## |
    hH6 = "h6"                ## |
    ## <h1>, <h2>, <h3>, <h4>, <h5>, <h6>  The HTML <h1>–<h6>
    ## elements represent six levels of section headings. <h1> is
    ## the highest section level and <h6> is the lowest.
    hHgroup = "hgroup"          ## |
    ## <hgroup>  The HTML <hgroup> element represents a multi-level
    ## heading for a section of a document. It groups a set of
    ## <h1>–<h6> elements.
    hMain = "main"              ## |
    ## <main>  The HTML <main> element represents the dominant
    ## content of the <body> of a document. The main content area
    ## consists of content that is directly related to or expands
    ## upon the central topic of a document, or the central
    ## functionality of an application.
    hNav = "nav"                ## |
    ## <nav>  The HTML <nav> element represents a section of a page
    ## whose purpose is to provide navigation links, either within
    ## the current document or to other documents. Common examples
    ## of navigation sections are menus, tables of contents, and
    ## indexes.
    hSection = "section"        ## |
    ## <section>  The HTML <section> element represents a
    ## standalone section — which doesn't have a more specific
    ## semantic element to represent it — contained within an HTML
    ## document.
    # Text content
    #
    # Use HTML text content elements to organize blocks or sections of content placed between the opening <body> and closing </body> tags. Important for accessibility and SEO, these elements identify the purpose or structure of that content.
    # Element  Description
    hBlockquote = "blockquote"  ## |
    ## <blockquote>  The HTML <blockquote> Element (or HTML Block
    ## Quotation Element) indicates that the enclosed text is an
    ## extended quotation. Usually, this is rendered visually by
    ## indentation (see Notes for how to change it). A URL for the
    ## source of the quotation may be given using the cite
    ## attribute, while a text representation of the source can be
    ## given using the <cite> element.
    hDd = "dd"                  ## |
    ## <dd>  The HTML <dd> element provides the description,
    ## definition, or value for the preceding term (<dt>) in a
    ## description list (<dl>).
    hDiv = "div"                ## |
    ## <div>  The HTML Content Division element (<div>) is the
    ## generic container for flow content. It has no effect on the
    ## content or layout until styled using CSS.
    hDl = "dl"                  ## |
    ## <dl>  The HTML <dl> element represents a description list.
    ## The element encloses a list of groups of terms (specified
    ## using the <dt> element) and descriptions (provided by <dd>
    ## elements). Common uses for this element are to implement a
    ## glossary or to display metadata (a list of key-value pairs).
    hDt = "dt"                  ## |
    ## <dt>  The HTML <dt> element specifies a term in a
    ## description or definition list, and as such must be used
    ## inside a <dl> element.
    hFigcaption = "figcaption"  ## |
    ## <figcaption>  The HTML <figcaption> or Figure Caption
    ## element represents a caption or legend describing the rest
    ## of the contents of its parent <figure> element.
    hFigure = "figure"          ## |
    ## <figure>  The HTML <figure> (Figure With Optional Caption)
    ## element represents self-contained content, potentially with
    ## an optional caption, which is specified using the
    ## (<figcaption>) element.
    hHr = "hr"                  ## |
    ## <hr>  The HTML <hr> element represents a thematic break
    ## between paragraph-level elements: for example, a change of
    ## scene in a story, or a shift of topic within a section.
    hLi = "li"                  ## |
    ## <li>  The HTML <li> element is used to represent an item in
    ## a list.
    hOl = "ol"                  ## |
    ## <ol>  The HTML <ol> element represents an ordered list of
    ## items — typically rendered as a numbered list.
    hP = "p"                    ## |
    ## <p>  The HTML <p> element represents a paragraph.
    hPre = "pre"                ## |
    ## <pre>  The HTML <pre> element represents preformatted text
    ## which is to be presented exactly as written in the HTML
    ## file.
    hUl = "ul"                  ## |
    ## <ul>  The HTML <ul> element represents an unordered list of
    ## items, typically rendered as a bulleted list.
    # Inline text semantics
    #
    # Use the HTML inline text semantic to define the meaning, structure, or style of a word, line, or any arbitrary piece of text.
    # Element  Description
    hA = "a"                    ## |
    ## <a>  The HTML <a> element (or anchor element), with its href
    ## attribute, creates a hyperlink to web pages, files, email
    ## addresses, locations in the same page, or anything else a
    ## URL can address.
    hAbbr = "abbr"              ## |
    ## <abbr>  The HTML Abbreviation element (<abbr>) represents an
    ## abbreviation or acronym; the optional title attribute can
    ## provide an expansion or description for the abbreviation.
    hB = "b"                    ## |
    ## <b>  The H Bring Attention To element (<b>) is used to
    ## draw the reader's attention to the element's contents, which
    ## are not otherwise granted special importance.
    hBdi = "bdi"                ## |
    ## <bdi>  The HTML Bidirectional Isolate element (<bdi>)  tells
    ## the browser's bidirectional algorithm to treat the text it
    ## contains in isolation from its surrounding text.
    hBdo = "bdo"                ## |
    ## <bdo>  The HTML Bidirectional Text Override element (<bdo>)
    ## overrides the current directionality of text, so that the
    ## text within is rendered in a different direction.
    hBr = "br"                  ## |
    ## <br>  The HTML <br> element produces a line break in text
    ## (carriage-return). It is useful for writing a poem or an
    ## address, where the division of lines is significant.
    hCite = "cite"              ## |
    ## <cite>  The HTML Citation element (<cite>) is used to
    ## describe a reference to a cited creative work, and must
    ## include the title of that work.
    hCode = "code"              ## |
    ## <code>  The HTML <code> element displays its contents styled
    ## in a fashion intended to indicate that the text is a short
    ## fragment of computer code.
    hData = "data"              ## |
    ## <data>  The HTML <data> element links a given piece of
    ## content with a machine-readable translation. If the content
    ## is time- or date-related, the <time> element must be used.
    hDfn = "dfn"                ## |
    ## <dfn>  The HTML Definition element (<dfn>) is used to
    ## indicate the term being defined within the context of a
    ## definition phrase or sentence.
    hEm = "em"                  ## |
    ## <em>  The HTML <em> element marks text that has stress
    ## emphasis. The <em> element can be nested, with each level of
    ## nesting indicating a greater degree of emphasis.
    hI = "i"                    ## |
    ## <i>  The HTML Idiomatic Text element (<i>) represents a
    ## range of text that is set off from the normal text for some
    ## reason, such as idiomatic text, technical terms, taxonomical
    ## designations, among others.
    hKbd = "kbd"                ## |
    ## <kbd>  The HTML Keyboard Input element (<kbd>) represents a
    ## span of inline text denoting textual user input from a
    ## keyboard, voice input, or any other text entry device.
    hMark = "mark"              ## |
    ## <mark>  The HTML Mark Text element (<mark>) represents text
    ## which is marked or highlighted for reference or notation
    ## purposes, due to the marked passage's relevance or
    ## importance in the enclosing context.
    hQ = "q"                    ## |
    ## <q>  The HTML <q> element indicates that the enclosed text
    ## is a short inline quotation. Most modern browsers implement
    ## this by surrounding the text in quotation marks.
    hRb = "rb"                  ## |
    ## <rb>  The HTML Ruby Base (<rb>) element is used to delimit
    ## the base text component of a  <ruby> annotation, i.e. the
    ## text that is being annotated.
    hRp = "rp"                  ## |
    ## <rp>  The HTML Ruby Fallback Parenthesis (<rp>) element is
    ## used to provide fall-back parentheses for browsers that do
    ## not support display of ruby annotations using the <ruby>
    ## element.
    hRt = "rt"                  ## |
    ## <rt>  The HTML Ruby Text (<rt>) element specifies the ruby
    ## text component of a ruby annotation, which is used to
    ## provide pronunciation, translation, or transliteration
    ## information for East Asian typography. The <rt> element must
    ## always be contained within a <ruby> element.
    hRtc = "rtc"                ## |
    ## <rtc>  The HTML Ruby Text Container (<rtc>) element embraces
    ## semantic annotations of characters presented in a ruby of
    ## <rb> elements used inside of <ruby> element. <rb> elements
    ## can have both pronunciation (<rt>) and semantic (<rtc>)
    ## annotations.
    hRuby = "ruby"              ## |
    ## <ruby>  The HTML <ruby> element represents small annotations
    ## that are rendered above, below, or next to base text,
    ## usually used for showing the pronunciation of East Asian
    ## characters. It can also be used for annotating other kinds
    ## of text, but this usage is less common.
    hS = "s"                    ## |
    ## <s>  The HTML <s> element renders text with a strikethrough,
    ## or a line through it. Use the <s> element to represent
    ## things that are no longer relevant or no longer accurate.
    ## However, <s> is not appropriate when indicating document
    ## edits; for that, use the <del> and <ins> elements, as
    ## appropriate.
    hSamp = "samp"              ## |
    ## <samp>  The HTML Sample Element (<samp>) is used to enclose
    ## inline text which represents sample (or quoted) output from
    ## a computer program.
    hSmall = "small"            ## |
    ## <small>  The HTML <small> element represents side-comments
    ## and small print, like copyright and legal text, independent
    ## of its styled presentation. By default, it renders text
    ## within it one font-size smaller, such as from small to
    ## x-small.
    hSpan = "span"              ## |
    ## <span>  The HTML <span> element is a generic inline
    ## container for phrasing content, which does not inherently
    ## represent anything. It can be used to group elements for
    ## styling purposes (using the class or id attributes), or
    ## because they share attribute values, such as lang.
    hStrong = "strong"          ## |
    ## <strong>  The HTML Strong Importance Element (<strong>)
    ## indicates that its contents have strong importance,
    ## seriousness, or urgency. Browsers typically render the
    ## contents in bold type.
    hSub = "sub"                ## |
    ## <sub>  The HTML Subscript element (<sub>) specifies inline
    ## text which should be displayed as subscript for solely
    ## typographical reasons.
    hSup = "sup"                ## |
    ## <sup>  The HTML Superscript element (<sup>) specifies inline
    ## text which is to be displayed as superscript for solely
    ## typographical reasons.
    hTime = "time"              ## |
    ## <time>  The HTML <time> element represents a specific period
    ## in time.
    hU = "u"                    ## |
    ## <u>  The HTML Unarticulated Annotation element (<u>)
    ## represents a span of inline text which should be rendered in
    ## a way that indicates that it has a non-textual annotation.
    hVar = "var"                ## |
    ## <var>  The HTML Variable element (<var>) represents the name
    ## of a variable in a mathematical expression or a programming
    ## context.
    hWbr = "wbr"                ## |
    ## <wbr>  The HTML <wbr> element represents a word break
    ## opportunity—a position within text where the browser may
    ## optionally break a line, though its line-breaking rules
    ## would not otherwise create a break at that location.
    # Image and multimedia
    #
    # HTML supports various multimedia resources such as images, audio, and video.
    # Element  Description
    hArea = "area"              ## |
    ## <area>  The HTML <area> tag defines an area inside an image
    ## map that has predefined clickable areas. An image map allows
    ## geometric areas on an image to be associated with hypertext
    ## link.
    hAudio = "audio"            ## |
    ## <audio>  The HTML <audio> element is used to embed sound
    ## content in documents. It may contain one or more audio
    ## sources, represented using the src attribute or the <source>
    ## element: the browser will choose the most suitable one. It
    ## can also be the destination for streamed media, using a
    ## MediaStream.
    hImg = "img"                ## |
    ## <img>  The HTML <img> element embeds an image into the
    ## document.
    hMap = "map"                ## |
    ## <map>  The HTML <map> element is used with <area> elements
    ## to define an image map (a clickable link area).
    hTrack = "track"            ## |
    ## <track>  The HTML <track> element is used as a child of the
    ## media elements, <audio> and <video>. It lets you specify
    ## timed text tracks (or time-based data), for example to
    ## automatically handle subtitles.
    hVideo = "video"            ## |
    ## <video>  The HTML Video element (<video>) embeds a media
    ## player which supports video playback into the document. You
    ## can use <video> for audio content as well, but the <audio>
    ## element may provide a more appropriate user experience.
    # Embedded content
    #
    # In addition to regular multimedia content, HTML can include a variety of other content, even if it's not always easy to interact with.
    # Element  Description
    hEmbed = "embed"            ## |
    ## <embed>  The HTML <embed> element embeds external content at
    ## the specified point in the document. This content is
    ## provided by an external application or other source of
    ## interactive content such as a browser plug-in.
    hIframe = "iframe"          ## |
    ## <iframe>  The HTML Inline Frame element (<iframe>)
    ## represents a nested browsing context, embedding another HTML
    ## page into the current one.
    hObject = "object"          ## |
    ## <object>  The HTML <object> element represents an external
    ## resource, which can be treated as an image, a nested
    ## browsing context, or a resource to be handled by a plugin.
    hParam = "param"            ## |
    ## <param>  The HTML <param> element defines parameters for an
    ## <object> element.
    hPicture = "picture"        ## |
    ## <picture>  The HTML <picture> element contains zero or more
    ## <source> elements and one <img> element to offer alternative
    ## versions of an image for different display/device scenarios.
    hSource = "source"          ## |
    ## <source>  The HTML <source> element specifies multiple media
    ## resources for the <picture>, the <audio> element, or the
    ## <video> element.
    # Scripting
    #
    # In order to create dynamic content and Web applications, HTML supports the use of scripting languages, most prominently JavaScript. Certain elements support this capability.
    # Element  Description
    hCanvas = "canvas"          ## |
    ## <canvas>  Use the HTML <canvas> element with either the
    ## canvas scripting API or the WebGL API to draw graphics and
    ## animations.
    hNoscript = "noscript"      ## |
    ## <noscript>  The HTML <noscript> element defines a section of
    ## HTML to be inserted if a script type on the page is
    ## unsupported or if scripting is currently turned off in the
    ## browser.
    hScript = "script"          ## |
    ## <script>  The HTML <script> element is used to embed
    ## executable code or data; this is typically used to embed or
    ## refer to JavaScript code.
    # Demarcating edits
    #
    # These elements let you provide indications that specific parts of the text have been altered.
    # Element  Description
    hDel = "del"                ## |
    ## <del>  The HTML <del> element represents a range of text
    ## that has been deleted from a document.
    hIns = "ins"                ## |
    ## <ins>  The HTML <ins> element represents a range of text
    ## that has been added to a document.
    # Table content
    #
    # The elements here are used to create and handle tabular data.
    # Element  Description
    hCaption = "caption"        ## |
    ## <caption>  The HTML <caption> element specifies the caption
    ## (or title) of a table.
    hCol = "col"                ## |
    ## <col>  The HTML <col> element defines a column within a
    ## table and is used for defining common semantics on all
    ## common cells. It is generally found within a <colgroup>
    ## element.
    hColgroup = "colgroup"      ## |
    ## <colgroup>  The HTML <colgroup> element defines a group of
    ## columns within a table.
    hTable = "table"            ## |
    ## <table>  The HTML <table> element represents tabular data —
    ## that is, information presented in a two-dimensional table
    ## comprised of rows and columns of cells containing data.
    hTbody = "tbody"            ## |
    ## <tbody>  The HTML Table Body element (<tbody>) encapsulates
    ## a set of table rows (<tr> elements), indicating that they
    ## comprise the body of the table (<table>).
    hTd = "td"                  ## |
    ## <td>  The HTML <td> element defines a cell of a table that
    ## contains data. It participates in the table model.
    hTfoot = "tfoot"            ## |
    ## <tfoot>  The HTML <tfoot> element defines a set of rows
    ## summarizing the columns of the table.
    hTh = "th"                  ## |
    ## <th>  The HTML <th> element defines a cell as header of a
    ## group of table cells. The exact nature of this group is
    ## defined by the scope and headers attributes.
    hThead = "thead"            ## |
    ## <thead>  The HTML <thead> element defines a set of rows
    ## defining the head of the columns of the table.
    hTr = "tr"                  ## |
    ## <tr>  The HTML <tr> element defines a row of cells in a
    ## table. The row's cells can then be established using a mix
    ## of <td> (data cell) and <th> (header cell) elements.
    # Forms
    #
    # HTML provides a number of elements which can be used together to create forms which the user can fill out and submit to the Web site or application. There's a great deal of further information about this available in the HTML forms guide.
    # Element  Description
    hButton = "button"          ## |
    ## <button>  The HTML <button> element represents a clickable
    ## button, used to submit forms or anywhere in a document for
    ## accessible, standard button functionality.
    hDatalist = "datalist"      ## |
    ## <datalist>  The HTML <datalist> element contains a set of
    ## <option> elements that represent the permissible or
    ## recommended options available to choose from within other
    ## controls.
    hFieldset = "fieldset"      ## |
    ## <fieldset>  The HTML <fieldset> element is used to group
    ## several controls as well as labels (<label>) within a web
    ## form.
    hForm = "form"              ## |
    ## <form>  The HTML <form> element represents a document
    ## section containing interactive controls for submitting
    ## information.
    hInput = "input"            ## |
    ## <input>  The HTML <input> element is used to create
    ## interactive controls for web-based forms in order to accept
    ## data from the user; a wide variety of types of input data
    ## and control widgets are available, depending on the device
    ## and user agent.
    hLabel = "label"            ## |
    ## <label>  The HTML <label> element represents a caption for
    ## an item in a user interface.
    hLegend = "legend"          ## |
    ## <legend>  The HTML <legend> element represents a caption for
    ## the content of its parent <fieldset>.
    hMeter = "meter"            ## |
    ## <meter>  The HTML <meter> element represents either a scalar
    ## value within a known range or a fractional value.
    hOptgroup = "optgroup"      ## |
    ## <optgroup>  The HTML <optgroup> element creates a grouping
    ## of options within a <select> element.
    hOption = "option"          ## |
    ## <option>  The HTML <option> element is used to define an
    ## item contained in a <select>, an <optgroup>, or a <datalist>
    ## element. As such, <option> can represent menu items in
    ## popups and other lists of items in an HTML document.
    hOutput = "output"          ## |
    ## <output>  The HTML Output element (<output>) is a container
    ## element into which a site or app can inject the results of a
    ## calculation or the outcome of a user action.
    hProgress = "progress"      ## |
    ## <progress>  The HTML <progress> element displays an
    ## indicator showing the completion progress of a task,
    ## typically displayed as a progress bar.
    hSelect = "select"          ## |
    ## <select>  The HTML <select> element represents a control
    ## that provides a menu of options
    hTextarea = "textarea"      ## |
    ## <textarea>  The HTML <textarea> element represents a
    ## multi-line plain-text editing control, useful when you want
    ## to allow users to enter a sizeable amount of free-form text,
    ## for example a comment on a review or feedback form.
    # Interactive elements
    #
    # HTML offers a selection of elements which help to create interactive user interface objects.
    # Element  Description
    hDetails = "details"        ## |
    ## <details>  The HTML Details Element (<details>) creates a
    ## disclosure widget in which information is visible only when
    ## the widget is toggled into an "open" state.
    hDialog = "dialog"          ## |
    ## <dialog>  The HTML <dialog> element represents a dialog box
    ## or other interactive component, such as a dismissable alert,
    ## inspector, or subwindow.
    hMenu = "menu"              ## |
    ## <menu>  The HTML <menu> element represents a group of
    ## commands that a user can perform or activate. This includes
    ## both list menus, which might appear across the top of a
    ## screen, as well as context menus, such as those that might
    ## appear underneath a button after it has been clicked.
    hSummary = "summary"        ## |
    ## <summary>  The HTML Disclosure Summary element (<summary>)
    ## element specifies a summary, caption, or legend for a
    ## <details> element's disclosure box.
    # Web Components
    #
    # Web Components is an HTML-related technology which makes it possible
    # to, essentially, create and use custom elements as if it were regular
    # HTML. In addition, you can create custom versions of standard HTML
    # elements. Element Description
    hSlot = "slot"              ## |
    ## <slot>  The HTML <slot> element—part of the Web Components
    ## technology suite—is a placeholder inside a web component
    ## that you can fill with your own markup, which lets you
    ## create separate DOM trees and present them together.
    hTemplate = "template"      ## |
    ## <template>  The HTML Content Template (<template>) element
    ## is a mechanism for holding HTML that is not to be rendered
    ## immediately when a page is loaded but may be instantiated
    ## subsequently during runtime using JavaScript.

  HtmlAttrKind* = enum
    atAccept           = "accept"            ## Specifies the types of files that the server accepts (only for type=""file"")"
    atAcceptCharset    = "accept-charset"    ## Specifies the character encodings that are to be used for the form submission
    atAccesskey        = "accesskey"         ## Specifies a shortcut key to activate/focus an element
    atAction           = "action"            ## Specifies where to send the form-data when a form is submitted
    atAlign            = "align"             ## Specifies the alignment according to surrounding elements. Use CSS instead
    atAlt              = "alt"               ## Specifies an alternate text when the original element fails to display
    atAsync            = "async"             ## Specifies that the script is executed asynchronously (only for external scripts)
    atAutocomplete     = "autocomplete"      ## Specifies whether the hForm or the hInput element should have autocomplete enabled
    atAutofocus        = "autofocus"         ## Specifies that the element should automatically get focus when the page loads
    atAutoplay         = "autoplay"          ## Specifies that the audio/video will start playing as soon as it is ready
    atBgcolor          = "bgcolor"           ## Specifies the background color of an element. Use CSS instead
    atBorder           = "border"            ## Specifies the width of the border of an element. Use CSS instead
    atCharset          = "charset"           ## Specifies the character encoding
    atChecked          = "checked"           ## Specifies that an hInput element should be pre-selected when the page loads (for type=""checkbox"" or type=""radio"")"
    atCite             = "cite"              ## Specifies a URL which explains the quote/deleted/inserted text
    atClass            = "class"             ## Specifies one or more classnames for an element (refers to a class in a style sheet)
    atColor            = "color"             ## Specifies the text color of an element. Use CSS instead
    atCols             = "cols"              ## Specifies the visible width of a text area
    atColspan          = "colspan"           ## Specifies the number of columns a table cell should span
    atContent          = "content"           ## Gives the value associated with the http-equiv or name attribute
    atContenteditable  = "contenteditable"   ## Specifies whether the content of an element is editable or not
    atControls         = "controls"          ## Specifies that audio/video controls should be displayed (such as a play/pause button etc)
    atCoords           = "coords"            ## Specifies the coordinates of the area
    atData             = "data"              ## Specifies the URL of the resource to be used by the object
    atData2            = "data2"             ## Used to store custom data private to the page or application
    atDatetime         = "datetime"          ## Specifies the date and time
    atDefault          = "default"           ## Specifies that the track is to be enabled if the user's preferences do not indicate that another track would be more appropriate
    atDefer            = "defer"             ## Specifies that the script is executed when the page has finished parsing (only for external scripts)
    atDir              = "dir"               ## Specifies the text direction for the content in an element
    atDirname          = "dirname"           ## Specifies that the text direction will be submitted
    atDisabled         = "disabled"          ## Specifies that the specified element/group of elements should be disabled
    atDownload         = "download"          ## Specifies that the target will be downloaded when a user clicks on the hyperlink
    atDraggable        = "draggable"         ## Specifies whether an element is draggable or not
    atEnctype          = "enctype"           ## Specifies how the form-data should be encoded when submitting it to the server (only for method=""post"")"
    atFor              = "for"               ## Specifies which form element(s) a label/calculation is bound to
    atForm             = "form"              ## Specifies the name of the form the element belongs to
    atFormaction       = "formaction"        ## Specifies where to send the form-data when a form is submitted. Only for type=""submit"""
    atHeaders          = "headers"           ## Specifies one or more headers cells a cell is related to
    atHeight           = "height"            ## Specifies the height of the element
    atHidden           = "hidden"            ## Specifies that an element is not yet, or is no longer, relevant
    atHigh             = "high"              ## Specifies the range that is considered to be a high value
    atHref             = "href"              ## Specifies the URL of the page the link goes to
    atHreflang         = "hreflang"          ## Specifies the language of the linked document
    atHttpEquiv        = "httpEquiv"         ## Provides an HTTP header for the information/value of the content attribute
    atId               = "id"                ## Specifieds a unique id for an element
    atIsmap            = "ismap"             ## Specifies an image as a server-side image map
    atKind             = "kind"              ## Specifies the kind of text track
    atLabel            = "label"             ## Specifies the title of the text track
    atLang             = "lang"              ## Specifies the language of the element's content
    atList             = "list"              ## Refers to a hDatalist element that contains pre-defined options for an hInput element
    atLoop             = "loop"              ## Specifies that the audio/video will start over again, every time it is finished
    atLow              = "low"               ## Specifies the range that is considered to be a low value
    atMax              = "max"               ## Specifies the maximum value
    atMaxlength        = "maxlength"         ## Specifies the maximum number of characters allowed in an element
    atMedia            = "media"             ## Specifies what media/device the linked document is optimized for
    atMethod           = "method"            ## Specifies the HTTP method to use when sending form-data
    atMin              = "min"               ## Specifies a minimum value
    atMultiple         = "multiple"          ## Specifies that a user can enter more than one value
    atMuted            = "muted"             ## Specifies that the audio output of the video should be muted
    atName             = "name"              ##  Specifies the name of the element
    atNovalidate       = "novalidate"        ## Specifies that the form should not be validated when submitted
    atOnabort          = "onabort"           ## Script to be run on abort
    atOnafterprint     = "onafterprint"      ## Script to be run after the document is printed
    atOnbeforeprint    = "onbeforeprint"     ## Script to be run before the document is printed
    atOnbeforeunload   = "onbeforeunload"    ## Script to be run when the document is about to be unloaded
    atOnblur           = "onblur"            ## Script to be run when the element loses focus
    atOncanplay        = "oncanplay"         ## Script to be run when a file is ready to start playing (when it has buffered enough to begin)
    atOncanplaythrough = "oncanplaythrough"  ## Script to be run when a file can be played all the way to the end without pausing for buffering
    atOnchange         = "onchange"          ## Script to be run when the value of the element is changed
    atOnclick          = "onclick"           ## Script to be run when the element is being clicked
    atOncontextmenu    = "oncontextmenu"     ## Script to be run when a context menu is triggered
    atOncopy           = "oncopy"            ## Script to be run when the content of the element is being copied
    atOncuechange      = "oncuechange"       ## Script to be run when the cue changes in a hTrack element
    atOncut            = "oncut"             ## Script to be run when the content of the element is being cut
    atOndblclick       = "ondblclick"        ## Script to be run when the element is being double-clicked
    atOndrag           = "ondrag"            ## Script to be run when the element is being dragged
    atOndragend        = "ondragend"         ## Script to be run at the end of a drag operation
    atOndragenter      = "ondragenter"       ## Script to be run when an element has been dragged to a valid drop target
    atOndragleave      = "ondragleave"       ## Script to be run when an element leaves a valid drop target
    atOndragover       = "ondragover"        ## Script to be run when an element is being dragged over a valid drop target
    atOndragstart      = "ondragstart"       ## Script to be run at the start of a drag operation
    atOndrop           = "ondrop"            ## Script to be run when dragged element is being dropped
    atOndurationchange = "ondurationchange"  ## Script to be run when the length of the media changes
    atOnemptied        = "onemptied"         ## Script to be run when something bad happens and the file is suddenly unavailable (like unexpectedly disconnects)
    atOnended          = "onended"           ## Script to be run when the media has reach the end (a useful event for messages like ""thanks for listening"")"
    atOnerror          = "onerror"           ## Script to be run when an error occurs
    atOnfocus          = "onfocus"           ## Script to be run when the element gets focus
    atOnhashchange     = "onhashchange"      ## Script to be run when there has been changes to the anchor part of the a URL
    atOninput          = "oninput"           ## Script to be run when the element gets user input
    atOninvalid        = "oninvalid"         ## Script to be run when the element is invalid
    atOnkeydown        = "onkeydown"         ## Script to be run when a user is pressing a key
    atOnkeypress       = "onkeypress"        ## Script to be run when a user presses a key
    atOnkeyup          = "onkeyup"           ## Script to be run when a user releases a key
    atOnload           = "onload"            ## Script to be run when the element is finished loading
    atOnloadeddata     = "onloadeddata"      ## Script to be run when media data is loaded
    atOnloadedmetadata = "onloadedmetadata"  ## Script to be run when meta data (like dimensions and duration) are loaded
    atOnloadstart      = "onloadstart"       ## Script to be run just as the file begins to load before anything is actually loaded
    atOnmousedown      = "onmousedown"       ## Script to be run when a mouse button is pressed down on an element
    atOnmousemove      = "onmousemove"       ## Script to be run as long as the  mouse pointer is moving over an element
    atOnmouseout       = "onmouseout"        ## Script to be run when a mouse pointer moves out of an element
    atOnmouseover      = "onmouseover"       ## Script to be run when a mouse pointer moves over an element
    atOnmouseup        = "onmouseup"         ## Script to be run when a mouse button is released over an element
    atOnmousewheel     = "onmousewheel"      ## Script to be run when a mouse wheel is being scrolled over an element
    atOnoffline        = "onoffline"         ## Script to be run when the browser starts to work offline
    atOnonline         = "ononline"          ## Script to be run when the browser starts to work online
    atOnpagehide       = "onpagehide"        ## Script to be run when a user navigates away from a page
    atOnpageshow       = "onpageshow"        ## Script to be run when a user navigates to a page
    atOnpaste          = "onpaste"           ## Script to be run when the user pastes some content in an element
    atOnpause          = "onpause"           ## Script to be run when the media is paused either by the user or programmatically
    atOnplay           = "onplay"            ## Script to be run when the media has started playing
    atOnplaying        = "onplaying"         ## Script to be run when the media has started playing
    atOnpopstate       = "onpopstate"        ## Script to be run when the window's history changes.
    atOnprogress       = "onprogress"        ## Script to be run when the browser is in the process of getting the media data
    atOnratechange     = "onratechange"      ## Script to be run each time the playback rate changes (like when a user switches to a slow motion or fast forward mode).
    atOnreset          = "onreset"           ## Script to be run when a reset button in a form is clicked.
    atOnresize         = "onresize"          ## Script to be run when the browser window is being resized.
    atOnscroll         = "onscroll"          ## Script to be run when an element's scrollbar is being scrolled
    atOnsearch         = "onsearch"          ## Script to be run when the user writes something in a search field (for hInput=""search"")"
    atOnseeked         = "onseeked"          ## Script to be run when the seeking attribute is set to false indicating that seeking has ended
    atOnseeking        = "onseeking"         ## Script to be run when the seeking attribute is set to true indicating that seeking is active
    atOnselect         = "onselect"          ## Script to be run when the element gets selected
    atOnstalled        = "onstalled"         ## Script to be run when the browser is unable to fetch the media data for whatever reason
    atOnstorage        = "onstorage"         ## Script to be run when a Web Storage area is updated
    atOnsubmit         = "onsubmit"          ## Script to be run when a form is submitted
    atOnsuspend        = "onsuspend"         ## Script to be run when fetching the media data is stopped before it is completely loaded for whatever reason
    atOntimeupdate     = "ontimeupdate"      ## Script to be run when the playing position has changed (like when the user fast forwards to a different point in the media)
    atOntoggle         = "ontoggle"          ## Script to be run when the user opens or closes the hDetails element
    atOnunload         = "onunload"          ## Script to be run when a page has unloaded (or the browser window has been closed)
    atOnvolumechange   = "onvolumechange"    ## Script to be run each time the volume of a video/audio has been changed
    atOnwaiting        = "onwaiting"         ## Script to be run when the media has paused but is expected to resume (like when the media pauses to buffer more data)
    atOnwheel          = "onwheel"           ## Script to be run when the mouse wheel rolls up or down over an element
    atOpen             = "open"              ## Specifies that the details should be visible (open) to the user
    atOptimum          = "optimum"           ## Specifies what value is the optimal value for the gauge
    atPattern          = "pattern"           ## Specifies a regular expression that an hInput element's value is checked against
    atPlaceholder      = "placeholder"       ## Specifies a short hint that describes the expected value of the element
    atPoster           = "poster"            ## Specifies an image to be shown while the video is downloading, or until the user hits the play button
    atPreload          = "preload"           ## Specifies if and how the author thinks the audio/video should be loaded when the page loads
    atReadonly         = "readonly"          ## Specifies that the element is read-only
    atRel              = "rel"               ## Specifies the relationship between the current document and the linked document
    atRequired         = "required"          ## Specifies that the element must be filled out before submitting the form
    atReversed         = "reversed"          ## Specifies that the list order should be descending (9,8,7...)
    atRows             = "rows"              ## Specifies the visible number of lines in a text area
    atRowspan          = "rowspan"           ## Specifies the number of rows a table cell should span
    atSandbox          = "sandbox"           ## Enables an extra set of restrictions for the content in an hIframe
    atScope            = "scope"             ## Specifies whether a header cell is a header for a column, row, or group of columns or rows
    atSelected         = "selected"          ## Specifies that an option should be pre-selected when the page loads
    atShape            = "shape"             ## Specifies the shape of the area
    atSize             = "size"              ## Specifies the width, in characters (for hInput) or specifies the number of visible options (for hSelect)
    atSizes            = "sizes"             ## Specifies the size of the linked resource
    atSpan             = "span"              ## Specifies the number of columns to span
    atSpellcheck       = "spellcheck"        ## Specifies whether the element is to have its spelling and grammar checked or not
    atSrc              = "src"               ## Specifies the URL of the media file
    atSrcdoc           = "srcdoc"            ## Specifies the HTML content of the page to show in the hIframe
    atSrclang          = "srclang"           ## Specifies the language of the track text data (required if kind=""subtitles"")"
    atSrcset           = "srcset"            ## Specifies the URL of the image to use in different situations
    atStart            = "start"             ## Specifies the start value of an ordered list
    atStep             = "step"              ## Specifies the legal number intervals for an input field
    atStyle            = "style"             ## Specifies an inline CSS style for an element
    atTabindex         = "tabindex"          ## Specifies the tabbing order of an element
    atTarget           = "target"            ## Specifies the target for where to open the linked document or where to submit the form
    atTitle            = "title"             ## Specifies extra information about an element
    atTranslate        = "translate"         ## Specifies whether the content of an element should be translated or not
    atType             = "type"              ## Specifies the type of element
    atUsemap           = "usemap"            ## Specifies an image as a client-side image map
    atValue            = "value"             ## Specifies the value of the element
    atWidth            = "width"             ## Specifies the width of the element
    atWrap             = "wrap"              ## Specifies how the text in a text area is to be wrapped when submitted in a form

    # atOnAutocomplete
    # atOnautocompleteerror
    atOnCancel
    atOnClose ## The close event fires when the user closes a <dialog>.
    atOndragexit
    atOnmouseenter
    atOnmouseleave
    atOnshow = "onshow" ## Execute a JavaScript when a <menu> element is shown as a context menu:
    atOnsort

type
  HTagSpec* = object
    tag: HtmlNodeKind
    attrs: seq[(HtmlAttrKind, string)]

  HAttrs* = openarray[(HtmlAttrKind, string)]

  HtmlNode* = ref object
    kind*: HtmlNodeKind
    text*: string
    subn*: seq[HtmlNode]

const
  hAllHtmlKinds*: set[HtmlNodeKind] = { low(HtmlNodeKind) .. high(HtmlNodeKind) }
  hAllVisibleKinds*: set[HtmlNodeKind] = hAllHtmlKinds - {
    hBase,  hBdo,  hBr, hHead, hHtml,  hIframe,  hMeta,  hParam,  hScript,  hStyle, hTitle}

  atEventHandlerKinds*: set[HtmlAttrKind] = {

    atOnabort,
    # atOnautocomplete, atOnautocompleteerror,
    atOnblur,
    atOncancel, atOncanplay, atOncanplaythrough, atOnchange, atOnclick,
    atOnclose, atOncontextmenu, atOncuechange, atOndblclick, atOndrag,
    atOndragend, atOndragenter,
    atOndragexit,
    atOndragleave, atOndragover,
    atOndragstart, atOndrop, atOndurationchange, atOnemptied, atOnended,
    atOnerror, atOnfocus, atOninput, atOninvalid, atOnkeydown,
    atOnkeypress, atOnkeyup, atOnload, atOnloadeddata, atOnloadedmetadata,
    atOnloadstart, atOnmousedown, atOnmouseenter, atOnmouseleave,
    atOnmousemove, atOnmouseout, atOnmouseover, atOnmouseup,
    atOnmousewheel, atOnpause, atOnplay, atOnplaying, atOnprogress,
    atOnratechange, atOnreset, atOnresize, atOnscroll, atOnseeked,
    atOnseeking, atOnselect, atOnshow, atOnsort, atOnstalled, atOnsubmit,
    atOnsuspend, atOntimeupdate, atOntoggle, atOnvolumechange, atOnwaiting,

  }

  allowedAttrMap*: array[HtmlAttrKind, set[HtmlNodeKind]] = toMapArray({
    atOnShow            : {hMenu},
    atAccept            : {hInput},
    atAcceptCharset     : {hForm},
    atAccesskey         : hAllHtmlKinds,
    atAction            : {hForm},
    atAlign             : {},
    atAlt               : {hArea, hImg, hInput},
    atAsync             : {hScript},
    atAutocomplete      : {hForm, hInput},
    atAutofocus         : {hButton, hInput, hSelect, hTextarea},
    atAutoplay          : {hAudio, hVideo},
    atBgcolor           : {},
    atBorder            : {},
    atCharset           : {hMeta, hScript},
    atChecked           : {hInput},
    atCite              : {hBlockquote, hDel, hIns, hQ},
    atClass             : hAllHtmlKinds,
    atColor             : {},
    atCols              : {hTextarea},
    atColspan           : {hTd, hTh},
    atContent           : {hMeta},
    atContenteditable   : hAllHtmlKinds,
    atControls          : {hAudio, hVideo},
    atCoords            : {hArea},
    atData              : {hObject},
    atData2             : hAllHtmlKinds,
    atDatetime          : {hDel, hIns, hTime},
    atDefault           : {hTrack},
    atDefer             : {hScript},
    atDir               : hAllHtmlKinds,
    atDirname           : {hInput, hTextarea},
    atDisabled          : {hButton, hFieldset, hInput, hOptgroup, hOption, hSelect, hTextarea},
    atDownload          : {hA, hArea},
    atDraggable         : hAllHtmlKinds,
    atEnctype           : {hForm},
    atFor               : {hLabel, hOutput},
    atForm              : {hButton, hFieldset, hInput, hLabel, hMeter, hObject, hOutput, hSelect, hTextarea},
    atFormaction        : {hButton, hInput},
    atHeaders           : {hTd, hTh},
    atHeight            : {hCanvas, hEmbed, hIframe, hImg, hInput, hObject, hVideo},
    atHidden            : hAllHtmlKinds,
    atHigh              : {hMeter},
    atHref              : {hA, hArea, hBase, hLink},
    atHreflang          : {hA, hArea, hLink},
    atHttpEquiv         : {hMeta},
    atId                : hAllHtmlKinds,
    atIsmap             : {hImg},
    atKind              : {hTrack},
    atLabel             : {hTrack, hOption, hOptgroup},
    atLang              : hAllHtmlKinds,
    atList              : {hInput},
    atLoop              : {hAudio, hVideo},
    atLow               : {hMeter},
    atMax               : {hInput, hMeter, hProgress},
    atMaxlength         : {hInput, hTextarea},
    atMedia             : {hA, hArea, hLink, hSource, hStyle},
    atMethod            : {hForm},
    atMin               : {hInput, hMeter},
    atMultiple          : {hInput, hSelect},
    atMuted             : {hVideo, hAudio},
    atName              : {hButton, hFieldset, hForm, hIframe, hInput, hMap,
                            hMeta, hObject, hOutput, hParam, hSelect, hTextarea},
    atNovalidate        : {hForm},
    atOnabort           : {hAudio, hEmbed, hImg, hObject, hVideo},
    atOnafterprint      : {hBody},
    atOnbeforeprint     : {hBody},
    atOnbeforeunload    : {hBody},
    atOnblur            : hAllVisibleKinds,
    atOncanplay         : {hAudio, hEmbed, hObject, hVideo},
    atOncanplaythrough  : {hAudio, hVideo},
    atOnchange          : hAllVisibleKinds,
    atOnclick           : hAllVisibleKinds,
    atOncontextmenu     : hAllVisibleKinds,
    atOncopy            : hAllVisibleKinds,
    atOncuechange       : {hTrack},
    atOncut             : hAllVisibleKinds,
    atOndblclick        : hAllVisibleKinds,
    atOndrag            : hAllVisibleKinds,
    atOndragend         : hAllVisibleKinds,
    atOndragenter       : hAllVisibleKinds,
    atOndragleave       : hAllVisibleKinds,
    atOndragover        : hAllVisibleKinds,
    atOndragstart       : hAllVisibleKinds,
    atOndrop            : hAllVisibleKinds,
    atOndurationchange  : {hAudio, hVideo},
    atOnemptied         : {hAudio, hVideo},
    atOnended           : {hAudio, hVideo},
    atOnerror           : {hAudio, hBody, hEmbed, hImg, hObject, hScript, hStyle, hVideo},
    atOnfocus           : hAllVisibleKinds,
    atOnhashchange      : {hBody},
    atOninput           : hAllVisibleKinds,
    atOninvalid         : hAllVisibleKinds,
    atOnkeydown         : hAllVisibleKinds,
    atOnkeypress        : hAllVisibleKinds,
    atOnkeyup           : hAllVisibleKinds,
    atOnload            : {hBody, hIframe, hImg, hInput, hLink, hScript, hStyle},
    atOnloadeddata      : {hAudio, hVideo},
    atOnloadedmetadata  : {hAudio, hVideo},
    atOnloadstart       : {hAudio, hVideo},
    atOnmousedown       : hAllVisibleKinds,
    atOnmousemove       : hAllVisibleKinds,
    atOnmouseout        : hAllVisibleKinds,
    atOnmouseover       : hAllVisibleKinds,
    atOnmouseup         : hAllVisibleKinds,
    atOnmousewheel      : hAllVisibleKinds,
    atOnoffline         : {hBody},
    atOnonline          : {hBody},
    atOnpagehide        : {hBody},
    atOnpageshow        : {hBody},
    atOnpaste           : hAllVisibleKinds,
    atOnpause           : {hAudio, hVideo},
    atOnplay            : {hAudio, hVideo},
    atOnplaying         : {hAudio, hVideo},
    atOnpopstate        : {hBody},
    atOnprogress        : {hAudio, hVideo},
    atOnratechange      : {hAudio, hVideo},
    atOnreset           : {hForm},
    atOnresize          : {hBody},
    atOnscroll          : hAllVisibleKinds,
    atOnsearch          : {hInput},
    atOnseeked          : {hAudio, hVideo},
    atOnseeking         : {hAudio, hVideo},
    atOnselect          : hAllVisibleKinds,
    atOnstalled         : {hAudio, hVideo},
    atOnstorage         : {hBody},
    atOnsubmit          : {hForm},
    atOnsuspend         : {hAudio, hVideo},
    atOntimeupdate      : {hAudio, hVideo},
    atOntoggle          : {hDetails},
    atOnunload          : {hBody},
    atOnvolumechange    : {hAudio, hVideo},
    atOnwaiting         : {hAudio, hVideo},
    atOnwheel           : hAllVisibleKinds,
    atOpen              : {hDetails},
    atOptimum           : {hMeter},
    atPattern           : {hInput},
    atPlaceholder       : {hInput, hTextarea},
    atPoster            : {hVideo},
    atPreload           : {hAudio, hVideo},
    atReadonly          : {hInput, hTextarea},
    atRel               : {hA, hArea, hForm, hLink},
    atRequired          : {hInput, hSelect, hTextarea},
    atReversed          : {hOl},
    atRows              : {hTextarea},
    atRowspan           : {hTd, hTh},
    atSandbox           : {hIframe},
    atScope             : {hTh},
    atSelected          : {hOption},
    atShape             : {hArea},
    atSize              : {hInput, hSelect},
    atSizes             : {hImg, hLink, hSource},
    atSpan              : {hCol, hColgroup},
    atSpellcheck        : hAllHtmlKinds,
    atSrc               : {hAudio, hEmbed, hIframe, hImg, hInput, hScript, hSource, hTrack, hVideo},
    atSrcdoc            : {hIframe},
    atSrclang           : {hTrack},
    atSrcset            : {hImg, hSource},
    atStart             : {hOl},
    atStep              : {hInput},
    atStyle             : hAllHtmlKinds,
    atTabindex          : hAllHtmlKinds,
    atTarget            : {hA, hArea, hBase, hForm},
    atTitle             : hAllHtmlKinds,
    atTranslate         : hAllHtmlKinds,
    atType              : {hA, hButton, hEmbed, hInput, hLink, hMenu, hObject, hScript, hSource, hStyle},
    atUsemap            : {hImg, hObject},
    atValue             : {hButton, hInput, hLi, hOption, hMeter, hProgress, hParam},
    atWidth             : {hCanvas, hEmbed, hIframe, hImg, hInput, hObject, hVideo},
    atWrap              : {hTextarea},
  })


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

type
  HtmlWriter* = object
    indentBuf: string
    stream: Stream



using writer: var HtmlWriter

proc newHtmlWriter*(stream: Stream): HtmlWriter =
  HtmlWriter(stream: stream)

proc newHtmlWriter*(file: AbsFile): HtmlWriter =
  newHtmlWriter(newFileStream(file, fmWrite))

proc newHtmlWriter*(file: File): HtmlWriter =
  newHtmlWriter(newFileStream(file))

proc space*(writer) = writer.stream.write(" ")
proc line*(writer) = writer.stream.write("\n")

proc write*(writer; text: varargs[string]) = writer.stream.write(text)
proc indent*(writer) = writer.indentBuf.add "  "
proc dedent*(writer) =
  writer.indentBuf.setLen(max(writer.indentBuf.len - 2, 0))

proc writeInd*(writer) = writer.stream.write(writer.indentBuf)


proc finish*(writer; elem: HtmlNodeKind; useLine: bool = true) =
  if useLine:
    writer.dedent(); writer.writeInd()

  writer.write("</", $elem, ">")
  if useLine:
    writer.line()

proc open*(writer; elem: HtmlNodeKind, useLine: bool = true) =
  if useLine: writer.writeInd()
  writer.write("<", $elem)

func htmlEscape*(str: string): string =
  # TODO
  str

proc attr*(writer; attr: HtmlAttrKind, val: string) =
  writer.write(&" {attr}={htmlEscape(val)}")

proc start*(
    writer; elem: HtmlNodeKind; attrs: HAttrs = @[], useLine: bool = true) =

  if useLine: writer.writeInd(); writer.indent()
  writer.write("<", $elem)
  for (key, val) in attrs:
    writer.attr(key, val)
  writer.write(">")
  if useLine: writer.line()

proc start*(writer; elem: openarray[HtmlNodeKind]) =
  writer.writeInd()
  for e in items(elem):
    writer.start(e, useLine = false)

  writer.indent()
  writer.line()

proc finish*(writer; elems: openarray[HtmlNodeKind]) =
  writer.dedent()
  writer.writeInd()
  for e in ritems(elems):
    writer.finish(e, useLine = false)

  writer.line()



proc initHTagSpec*(tag: HtmlNodeKind, args: HAttrs = @[]): HTagSpec =
  HTagSpec(tag: tag, attrs: toSeq(args))

macro `</`*(args: untyped): untyped =
  result = nnkBracket.newTree()
  for arg in args:
    case arg.kind:
      of nnkIdent:
        result.add newCall("initHTagSpec", arg)

      of nnkCurlyExpr:
        result.add newCall(
          "initHTagSpec", arg[0],
          nnkTableConstr.newTree(toSeq(arg[1 .. ^1])))

      else:
        result.add newCall("initHTagSpec", toSeq(arg))

proc start*(writer; elem: openarray[HTagSpec]) =
  writer.writeInd()
  for e in items(elem):
    writer.start(e.tag, e.attrs, useLine = false)

  writer.indent()
  writer.line()

proc finish*(writer; elems: openarray[HTagSpec]) =
  writer.dedent()
  writer.writeInd()
  for e in ritems(elems):
    writer.finish(e.tag, useLine = false)

  writer.line()


proc close*(writer) = writer.stream.write(">")
proc text*(writer; t: varargs[string]) = writer.stream.write(t)

proc closeEnd*(writer) =
  writer.stream.write("/>")

proc header*(writer) =
  writer.write("<!DOCTYPE html>\n")

const hInlineKinds* = {hB, hStrong, hI}

template wrap*(writer; t: HtmlNodeKind, body: untyped): untyped =
  writer.start(t)
  try: body finally: writer.finish(t)

template wrap*[N](writer; t: array[N, HtmlNodeKind], body: untyped): untyped =
  writer.start(t)
  try: body finally: writer.finish(t)

template wrap*[N](writer; t: array[N, HTagSpec], body: untyped): untyped =
  writer.start(t)
  try: body finally: writer.finish(t)

proc style*(writer; css: openarray[(string, seq[(string, string)])]) =
  writer.start(hStyle)
  for (select, properties) in css:
    writer.writeInd()
    writer.write(&"{select} ", "{")
    writer.line()
    writer.indent()
    for (key, val) in properties:
      writer.writeInd()
      writer.write(key, ": ", val, ";")
      writer.line()

    writer.dedent()

    writer.writeInd()
    writer.write("}")
    writer.line()

  writer.finish(hStyle)

const
  hRow* = hTr
  hCell* = hTh

template wrap*(writer; t: HtmlNodeKind, attrs: HAttrs, body: untyped): untyped =
  writer.start(t, attrs)
  try:
    body
  finally:
    writer.finish(t)


template wrap0*(writer; t: HtmlNodeKind; body: untyped): untyped =
  writer.writeInd()
  writer.start(t, useLine = false)
  try:
    body
  finally:
    writer.finish(t, false)
    writer.line()

proc link*(writer; url: string; text: string; class: string = "") =
  writer.open(hA, false)
  writer.attr(atHref, url)
  if class != "":
    writer.attr(atClass, class)
  writer.close()
  writer.text(text)
  writer.finish(hA, false)
