import
  hmisc/wrappers/wraphelp

wrapheader "<string>":
  namespace "std":
    class "string" as StdString:
      proc len(): cint = size()

var str: StdString
echo str.len()
