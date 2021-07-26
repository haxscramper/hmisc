import
  hmisc/wrappers/wraphelp

wrapheader "<string>":
  namespace "std":
    class "string" as StdString:
      proc len(): CxxTemplateApproximate[int] {.`const`.} = size()
      proc resize(n: CxxTemplateApproximate[int])
      proc `+=`(n: char)
      proc `[]`(idx: CxxTemplateApproximate[int]): char

wrapheader "<stdexcept>":
  namespace "std":
    class "logic_error" as StdLogicError of StdException


proc raiseLogic() {.raises: [StdLogicError].} =
  {.emit:
    "throw std::logic_error(\"test message from raised C++ exception\");"}

proc main() =
  var str: StdString
  echo "`as` conversion: ", str.len() as int
  let test: int = str.len()
  echo "apprximate converter: ", test
  str += '2'
  echo "value at [0]: ", str[0]

proc testTry(arg: int) =
  try:
    echo "12"
    if arg == 1:
      raiseLogic()

  except StdLogicError as e:
    echo e.what()

testTry(1)
main()
