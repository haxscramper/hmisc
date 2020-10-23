import sugar, strutils, sequtils, strformat

import unittest

import hmisc/other/colorlogger

startColorLogger()

suite "Colorlogger":
  test "test":
    debug "debug"
    info "info"
    notice "notice"
    warn "warn"
    err "err"
    fatal "fatal"
