import
  hmisc/wrappers/[wraphelp, wraphelp_gen, wraphelp_decl],
  hmisc/core/all

{.passc: "-fPIC".}
{.passc: "-I/usr/include/qt".}
{.passc: "-I/usr/include/qt/QtGui".}
{.passc: "-I/usr/include/qt/QtWidgets".}

{.passl: "-lQt5Gui".}
{.passl: "-lQt5Core".}
{.passl: "-lQt5Widgets".}

wrapheader "<QWidget>":
  class QWidget

wrapheader "<QTextEdit>":
  class QTextEdit of QWidget:
    proc newQTextEdit(parent: ptr QWidget): ptr QTextEdit {.constructor.}

wrapheader "<QMainWindow>":
  class QMainWindow of QWidget:
    proc newQMainWindow(): ptr QMainWindow {.constructor.}
    proc show()
    proc setCentralWidget(widget: ptr QWidget)

wrapheader "<QApplication>":
  class QApplication:
    proc newQApplication(argc: cint, argv: cstringArray):
      ptr QApplication {.constructor.}

    proc exec()

static: startHaxComp()

cgen "${cacheDir}/${file}":
  include "<QTextEdit>"

  class DerivedEditor of QTextEdit:
    proc textChanged() {.override, slot.} =
      echo "Signal derived changed"

    proc new(parent: ptr QWidget): ptr DerivedEditor
      {.constructor(QTextEdit(parent)).}

var
  argc: cint = 0
  argv: cstringarray
  app = newQApplication(argc, argv)
  window = newQMainWindow()
  edit = newDerivedEditor(window)



# cgenIni

# if false:
#   window[].show()
#   window[].setCentralWidget(edit)

#   app[].exec()
