import terminal, sequtils, strutils, helpers, unicode

{.deprecated: "Scheduled for removal".}

proc justifyFitTerminal*(
  str: string,
  padding: tuple[left: int, right: int] = (0,0),
  maxWidth: int = 80): seq[string] =
    let width = min(terminalWidth(), maxWidth) -
    (padding.left + padding.right)

    let strSplit = str
    .replace("\n", " ")
    .split(' ')
    .mapIt(unicode.strip(it))
    .filterIt(it.len != 0)

    var buf: string

    for s in strSplit:
      if buf.runelen + s.runelen <= width:
        buf.add(tern(buf.runelen == 0, "", " ") & s)
      else:
        result.add(buf)
        buf = s

    result.add(buf)


when isMainModule:
  echo """
  Normal tracking mode (not implemented in Linux 2.0.24) sends an
  escape sequence on both button press and release.  Modifier
  information is also sent.  It is enabled by sending ESC [ ? 1000 h
  and disabled with ESC [ ? 1000 l.  On button press or release,
  xterm(1) sends ESC [ M bxy.  The low two bits of b encode button
  information: 0=MB1 pressed, 1=MB2 pressed, 2=MB3 pressed, 3=release.
  The upper bits encode what modifiers were down when the button was
  pressed and are added together: 4=Shift, 8=Meta, 16=Control.  Again x
  and y are the x and y coordinates of the mouse event.  The upper left
  corner is (1,1).
  """.justifyFitTerminal((10,10)).join("\n")
