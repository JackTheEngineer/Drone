## Definitions for the local Test-Compilations Process
require File.expand_path('./common_definitions.rb', File.dirname(__FILE__))


#===========================================
# LOCAL Matchine Defs for tests
#===========================================
LOCAL_GCC = "gcc"
LOCAL_COMPILER_OPTIONS = [
  '-std=gnu11',
#  '-Wall',
  '-Wextra',
  '-Werror',
  '-Wpointer-arith',
  '-Wcast-align',
  '-Wwrite-strings',
  '-Wswitch-default',
  '-Wunreachable-code',
  '-Winit-self',
  '-Wmissing-field-initializers',
  '-Wno-unknown-pragmas',
  '-Wstrict-prototypes',
  '-Wundef',
  '-Wold-style-definition',
  '-Wmissing-prototypes',
  '-Wmissing-declarations',
  '-Wno-unused-variable',
  '-Wno-unused-parameter',
  '-Wno-pointer-to-int-cast',
  '-Wno-unused-but-set-variable',
  '-Wno-sign-compare',
  '-Wno-implicit-function-declaration',
  '-Wno-missing-prototypes',
  '-Wno-missing-declarations',
  '-g3 -O0',
  '-MD',  # Generate dependency files'
].join(" ")

LOCAL_LINK_OPTIONS = [
  '-g3'
].join(" ")

LOCAL_LINK_COMMAND = [
  "#{LOCAL_GCC}",
  "#{LOCAL_LINK_OPTIONS}"
].join(" ")


