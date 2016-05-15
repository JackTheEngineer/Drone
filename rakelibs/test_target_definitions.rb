## Definitions for the local Test-Compilations Process
require File.expand_path('./common_definitions.rb', File.dirname(__FILE__))


#===========================================
# LOCAL Matchine Defs for tests
#===========================================
LOCAL_GCC = "gcc"
LOCAL_COMPILER_OPTIONS = [
  '-std=gnu11',
  '$(gsl-config --cflags)',
  '-Wall',
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
  # '-Wno-unused-variable',
  '-Wno-unused-parameter',
  # '-Wno-pointer-to-int-cast',
  # '-Wno-unused-but-set-variable',
  # '-Wno-sign-compare',
  # '-Wno-implicit-function-declaration',
  # '-Wno-missing-prototypes',
  # '-Wno-missing-declarations',
  '-DUNITY_INCLUDE_DOUBLE=1',
  '-g3 -O0',
].join(" ")

LOCAL_GCC_COMPILE_LIBS =  [
  '$(gsl-config --libs)',
].join(" ")

LOCAL_LINK_OPTIONS = [
  '-g3'
].join(" ")

LOCAL_LINK_COMMAND = [
  "#{LOCAL_GCC}",
  "#{LOCAL_LINK_OPTIONS}"
].join(" ")


