# import libfswatch, libfswatch/fswatch
import oswrap
import std/[bitops, strformat]
import fusion/pointers

const libfswatch =
  when defined(windows): "libfswatch.dll"
  elif defined(macosx):  "libfswatch.dylib"
  else:                  "libfswatch.so"


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



  FswCmonitorFilter* = object
    text*: string
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

proc addFilter*(handle: FswHandle, filter: FswCmonitorFilter): cint
  {.importc: "fsw_add_filter".}
  ## Adds a filter to the current session. A filter is a regular expression
  ## that, depending on whether the filter type is exclusion or not, must
  ## or must not be matched for an event path for the event to be accepted.

proc startMonitor*(handle: FswHandle): cint
  {.importc: "fsw_start_monitor".}

proc stopMonitor*(handle: FswHandle): cint
  {.importc: "fsw_stop_monitor".}

proc isRunning*(handle: FswHandle): bool
  {.importc: "fsw_is_running".}

proc destroySession*(handle: FswHandle): cint
  {.importc: "fsw_destroy_session".}

proc fswLastError*(): cint {.importc: "fsw_last_error".}
proc fswIsVerbose*(): bool {.importc: "fsw_is_verbose".}
proc fswSetVerbose*(verbose: bool) {.importc: "fsw_set_verbose".}

type
  Monitor* = object
    handle*: FswHandle

  FsWatchError = object of CatchableError

proc addEventTypeFilter*(monitor: Monitor, types: set[FswEventFlag]) =
  for filterType in types:
    # WARNING discarding return code
    discard monitor.handle.addEventTypeFilter(
      FswEventTypeFilter(flag: filterType))



proc newMonitor*(): Monitor =
  if fsw_init_library() != 0:
    raise newException(
      FsWatchError, "Failed to initalized libfswatch library")

  result.handle = fsw_init_session(0)

proc addPath*(monitor: Monitor, path: string) =
  if monitor.handle.add_path(path) != 0:
    raise newException(
      FsWatchError, &"Failed to add path {path} to monitor")


proc setCallback*(monitor: Monitor, callback: proc(event: FswEvent)) =
  let callbackWrap = proc(events: ptr FswEvent, eventNum: cint) =
    let arr = toUncheckedArray(events)
    for i in 0 ..< eventNum:
      callback(arr[i])

  let callbackEnv: pointer = rawEnv(callbackWrap)
  let callbackImpl = cast[FswEventCallback](rawProc(callbackWrap))

  if monitor.handle.setCallback(callbackImpl, callbackEnv) != 0:
    raise newException(
      FsWatchError, &"Failed to set callback to monitor")

proc start*(monitor: Monitor) =
  discard monitor.handle.start_monitor()

func contains[T](superset: set[T], subset: set[T]): bool =
  len(subset - superset) == 0

proc newMonitor*(path: AnyPath, cb: proc(event: FswEvent)): Monitor =
  result = newMonitor()
  result.addPath(path.getStr())
  result.setCallback do(event: FswEvent):
    cb(event)

template newMonitorEvent*(path: AnyPath, body: untyped): untyped =
  newMonitor(
    path,
    proc(event {.inject.}: FswEvent) =
      body
  )

when isMainModule:
  var monitor = newMonitorEvent(AbsDir("/tmp")):
    echo event

  monitor.addEventTypeFilter({fefCreated, fefUpdated})
  monitor.start()
