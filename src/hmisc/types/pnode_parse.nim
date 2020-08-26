import compiler/[parser, llstream, idents, options, pathutils, astalgo]
import compiler/[ast]

proc parsePNodeStr*(str: string): PNode =
  let cache: IdentCache = newIdentCache()
  let config: ConfigRef = newConfigRef()
  var pars: TParser

  openParser(
    p = pars,
    filename = AbsoluteFile(currentSourcePath()),
    inputStream = llStreamOpen(str),
    cache = cache,
    config = config
  )

  result = parseAll(pars)
  closeParser(pars)
