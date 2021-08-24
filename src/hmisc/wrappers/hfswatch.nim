import
  ../other/oswrap,
  ../other/rx,
  ../core/all

import std/[bitops, strformat]
import fusion/pointers

const libfswatch =
  when defined(windows): "libfswatch.dll"
  elif defined(macosx):  "libfswatch.dylib"
  else:                  "libfswatch.so"

##[

## Path filtering

From libfswatch documentation https://emcrisostomo.github.io/fswatch/doc/1.14.0/libfswatch.html/path-filtering.html

    A filter type (fsw_filter_type) determines whether the filter regular
    expression is used to include and exclude paths from the list of the
    events processed by the library. libfswatch processes filters this way:

    - If a path matches an including filter, the path is accepted no matter
      any other filter.
    - If a path matches an excluding filter, the path is rejected.
    - If a path matches no lters, the path is accepted.

    Said another way:

    - All paths are accepted by default, unless an exclusion filter
      says otherwise.
    - Inclusion filters may override any other exclusion filter.
    - The order in the filter definition has no effect.

To simplify creation of regular expressions (especially considering two
different flavors can be specified, each one different from pcre that nim
uses) it is recommended to use `rx` helper

]##


type
  FswHandle* = ptr object
  FswEventFlag* = enum
    fefNoOp = 0 ## No event has occurred
    fefPlatformSpecific = 1 shl 0 ## Platform-specific placeholder for event
                               ## type that cannot currently be mapped
    fefCreated = 1 shl 1 ## An object was created
    fefUpdated = 1 shl 2 ## An object was updated
    fefRemoved = 1 shl 3 ## An object was removed
    fefRenamed = 1 shl 4 ## An object was renamed
    fefOwnerModified = 1 shl 5 ## The owner of an object was modified
    fefAttributeModified = 1 shl 6 ## The attributes of an object were modified
    fefMovedFrom = 1 shl 7 ## An object was moved from this location
    fefMovedTo = 1 shl 8 ## An object was moved to this location
    fefIsFile = 1 shl 9 ## The object is a file
    fefIsDir = 1 shl 10 ## The object is a directory
    fefIsSymLink = 1 shl 11 ## The object is a symbolic link
    fefLink = 1 shl 12 ## The link count of an object has changed
    fefOverflow = 1 shl 13 ##The event queue has overflowed.

  FswEvent* = object
    path*: cstring ## where the event was triggered.
    time_t*: cint ## Unix timestamp for time when the event was triggered.
    flags*: ptr FswEventFlag ## array of fsw_event_flag of size flags_num.
    flags_num*: cuint ## size of the flags array.

  FswEventTypeFilter* = object
    flag*: FswEventFlag

  FswFilterType* = enum
    filterInclude = 0
    filterExclude = 1

  FswEventCallback =
    proc(events: ptr FswEvent, eventNum: cuint, data: pointer) {.cdecl.}
    ##[

A function pointer of type FSW_CEVENT_CALLBACK is used by the API as a
callback to provide information about received events.

The callback is passed the following arguments:

- @arg{events} :: a const pointer to an array of events of type
  `const FswCevent`.
- @arg{eventNum} :: the size of the *events array.
- @arg{data} :: optional persisted data for a callback.

The memory used by the fsw_cevent objects will be freed at the end of the
callback invocation. A callback should copy such data instead of storing a
pointer to it.


]##



  FswFsMonitorFilter* = object
    text*: cstring
    filterType*: FswFilterType
    caseSensitive*: bool
    extended*: bool

proc getFlags*(ev: FswEvent): set[FswEventFlag] =
  var flags = ev.flags[]
  for i in 0 .. 13:
    if bitand(1 shl i, cast[int](flags)) != 0:
      result.incl FswEventFlag(1 shl i)

proc `$`(event: FswEvent): string =
  result = &"(path: \"{event.path}\", time: {event.time_t}, "
  result &= &"flags: {event.getFlags()})"

{.push dynlib: libfswatch.}

proc fswInitLibrary*(): cint
  {.importc: "fsw_init_library".}

proc fswInitSession*(monitor_type: cint): FswHandle
  {.importc: "fsw_init_session".}

proc addPath*(handle: FswHandle, path: cstring): cint
  {.importc: "fsw_add_path".}
  ## Adds a path to watch to the specified session. At least one path must
  ## be added to the current session in order for it to be valid.


proc addProperty*(handle: FswHandle, name: string, value: string): cint
  {.importc: "fsw_add_property".}
  ## Adds the specified monitor property.

proc setAllowOverflow*(handle: FswHandle, allow_overflow: bool): cint
  {.importc: "fsw_set_allow_overflow".}
  ## Sets the allow overflow flag of the monitor. When this flag is set, a
  ## monitor is allowed to overflow buffer and report it as a change event.



proc setCallback*(handle: FswHandle, callback: FswEventCallback, data: pointer): cint
  {.importc: "fsw_set_callback".}
  ## Sets the callback the monitor invokes when some events are received.
  ## The callback must be set in the current session in order for it to be
  ## valid.

proc setLatency*(handle: FswHandle, latency: cdouble): cint
  {.importc: "fsw_set_latency".}
  ## Sets the latency of the monitor. By default, the latency is set to 1 s.

proc setRecursive*(handle: FswHandle, recursive: bool): cint
  {.importc: "fsw_set_recursive".}
  ## Determines whether the monitor recursively scans each watched path or
  ## not. Recursive scanning is an optional feature which could not be
  ## implemented by all the monitors. By default, recursive scanning is
  ## disabled.



proc setDirectoryOnly*(handle: FswHandle, directory_only: bool): cint
  {.importc: "fsw_set_directory_only".}
  ## Determines whether the monitor only watches a directory when
  ## performing a recursive scan. By default, a monitor accepts all kinds
  ## of files.



proc setFollowSymlinks*(handle: FswHandle, follow_symlinks: bool): cint
  {.importc: "fsw_set_follow_symlinks".}
  ## Determines whether a symbolic link is followed or not. By default, a
  ## symbolic link are not followed.



proc addEventTypeFilter*(handle: FswHandle, event_type: FswEventTypeFilter): cint
  {.importc: "fsw_add_event_type_filter".}
  ## Adds an event type filter to the current session.

proc addFilter*(handle: FswHandle, filter: FswFsMonitorFilter): cint
  {.importc: "fsw_add_filter".}
  ## Adds a filter to the current session. A filter is a regular expression
  ## that, depending on whether the filter type is exclusion or not, must
  ## or must not be matched for an event path for the event to be accepted.

proc startFsMonitor*(handle: FswHandle): cint
  {.importc: "fsw_start_monitor".}

proc stopFsMonitor*(handle: FswHandle): cint
  {.importc: "fsw_stop_monitor".}

proc isRunning*(handle: FswHandle): bool
  {.importc: "fsw_is_running".}

proc destroySession*(handle: FswHandle): cint
  {.importc: "fsw_destroy_session".}

proc fswLastError*(): cint {.importc: "fsw_last_error".}
proc fswIsVerbose*(): bool {.importc: "fsw_is_verbose".}
proc fswSetVerbose*(verbose: bool) {.importc: "fsw_set_verbose".}

type
  FsMonitor* = object
    handle*: FswHandle

  FsWatchError = object of CatchableError

proc addEventTypeFilter*(monitor: FsMonitor, types: set[FswEventFlag]) =
  for filterType in types:
    # WARNING discarding return code
    discard monitor.handle.addEventTypeFilter(
      FswEventTypeFilter(flag: filterType))



proc newFsMonitor*(): FsMonitor =
  if fsw_init_library() != 0:
    raise newException(
      FsWatchError, "Failed to initalized libfswatch library")

  result.handle = fsw_init_session(0)

proc addPath*(monitor: FsMonitor, path: string) =
  if monitor.handle.add_path(path) != 0:
    raise newException(
      FsWatchError, &"Failed to add path {path} to monitor")


proc setCallback*(monitor: FsMonitor, callback: proc(event: FswEvent)) =
  let callbackWrap = proc(events: ptr FswEvent, eventNum: cint) =
    let arr = toUncheckedArray(events)
    for i in 0 ..< eventNum:
      callback(arr[i])

  let callbackEnv: pointer = rawEnv(callbackWrap)
  let callbackImpl = cast[FswEventCallback](rawProc(callbackWrap))

  if monitor.handle.setCallback(callbackImpl, callbackEnv) != 0:
    raise newException(
      FsWatchError, &"Failed to set callback to monitor")

proc addFilter*(
    monitor: FsMonitor, regex: Rx, inclusive: bool = false,
    ignorecase: bool = false, extended: bool = true
  ) =

  let str: string = toStr(
    regex, if extended: rxfExtendedPosix else: rxfPosix)

  discard monitor.handle.addFilter(FswFsMonitorFilter(
    text: str.cstring,
    extended: extended,
    filterType: (if inclusive: filterInclude else: filterExclude),
    caseSensitive: not ignorecase
  ))

proc setFilterOnly*(monitor: FsMonitor, regexList: seq[Rx]) =
  ## Accept only paths matching `regexList`
  ##
  ## - FIXME :: I don't know why, but current implementation simply does
  ##   not work. I want to have this as a part of an API, so I won't be
  ##   removing it, but for now all paths are to be filtered out manually.
  monitor.addFilter(*nrx(rskAny), inclusive = false)
  for regex in regexList:
    monitor.addFilter(regex, inclusive = true)


proc start*(monitor: FsMonitor) =
  discard monitor.handle.startFsMonitor()

func contains[T](superset: set[T], subset: set[T]): bool =
  len(subset - superset) == 0

proc newFsMonitor*(path: AnyPath, cb: proc(event: FswEvent)): FsMonitor =
  result = newFsMonitor()
  result.addPath(path.getStr())
  result.setCallback do(event: FswEvent):
    cb(event)

template newFsMonitorEvent*(path: AnyPath, body: untyped): untyped =
  newFsMonitor(
    path,
    proc(event {.inject.}: FswEvent) =
      body
  )

when isMainModule:
  starthax()
  let frx = nrx("/tmp").fullLine()
  var monitor = newFsMonitorEvent(AbsDir("/tmp")):
    echov event, " ", $event.path =~ frx

  monitor.addFilter(frx)
  monitor.start()
