import
  hmisc/wrappers/wraphelp

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

var
  argc: cint = 0
  argv: cstringarray
  app = newQApplication(argc, argv)
  window = newQMainWindow()
  edit = newQTextEdit(window)


if false:
  window[].show()
  window[].setCentralWidget(edit)

  app[].exec()
