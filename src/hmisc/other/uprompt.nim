import strutils, sequtils, strformat, parseutils, options

# [#A] TODO add support for exiting without selecting anything

# IDEA Add support for highlightin todo comments. Things like tags,
# importance levels etc. Better wrapping (do not ignore lists)

proc writePrompt(s: string): void =
  stdout.write(" |- " & s & "\n    ")


proc getInt*(
  prompt: string,
  valRange: (int, int) = (0, 100),
  defaultTo: Option[int] = some(0)): int =
  var hasErrors = true

  let rangeStr = "[$# $#]" % [$valRange[0], $valRange[1]]

  while hasErrors:
    writePrompt(s = prompt & " in range: " & rangeStr )
    let input = stdin.readLine()
    echo ""
    if defaultTo.isSome() and input.len == 0:
      hasErrors = false
      return defaultTo.get()

    try:
      result = input.parseInt()
      if valRange[0] <= result and result <= valRange[1]:
        hasErrors = false
      else:
        echo "Value not in range: ", rangeStr
    except:
      echo "Fuck you"

proc promptYN*(prompt: string, default: string = "Y"): bool =
  var hasErrors = true
  while hasErrors:
    writePrompt(prompt & " [Y/n] ")
    var input = stdin.readLine()
    echo ""

    if input.len == 0: input = default

    if input in @["Y", "y", "1"]:
      return true
    elif input in @["N", "n", "0"]:
      return false
    else:
      echo "Enter y/n"


# TODO select item from sequence of items or tuples. Provide
# pretty-print (add support for custom printing solution (write
# template wrapper?))

# TODO @idea: use code for checking string/enum values, parsing tuples
# etc. that is/would be used in argparse/argcheck to generate better
# error messages/more functional parsing tooling
