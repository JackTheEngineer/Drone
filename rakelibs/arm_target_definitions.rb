## Definitions for the Compilations for the ARM Targets
require File.expand_path('./common_definitions.rb', File.dirname(__FILE__))


ARM_BASE = '/home/jakov/bin/gcc-arm-none-eabi-4_9-2014q4/bin/'
ARM_GCC = "#{ARM_BASE}arm-none-eabi-gcc"
ARM_OBJCOPY = "#{ARM_BASE}arm-none-eabi-objcopy"
ARM_SIZE = "#{ARM_BASE}arm-none-eabi-size"
# =======================================================================
#  ARM Assembler Options Definition
# =======================================================================
ARM_ASM_OPTIONS = [
  '-x',
  'assembler-with-cpp',
  '-DXMC4500_F100x1024',
  '-mfloat-abi=softfp',
  '-c',
  '-fmessage-length=0',
  '-mcpu=cortex-m4',
  '-mfpu=fpv4-sp-d16',
  '-mthumb',
  '-g',
  '-gdwarf-2',
].join(" ")

# =======================================================================
#  ARM Compiler Definition
# =======================================================================

ARM_COMPILE_OPTIONS = [
  '-DXMC4500_F100x1024',
  '-O0',
  '-ffunction-sections',
  '-fdata-sections',
  '-Wall',
  '-std=gnu99',
  '-mfloat-abi=softfp',
  '-pipe',
  '-fmessage-length=0',
  '-mcpu=cortex-m4',
  '-mfpu=fpv4-sp-d16',
  '-mthumb',
  '-g',
  '-gdwarf-2',
].join(' ')


# =======================================================================
# ARM Linker Definition
# =======================================================================
ARM_LINK_OPTIONS = [
'-nostartfiles',
'-Xlinker',
'--gc-sections', 
'-mfloat-abi=softfp', 
'-mfpu=fpv4-sp-d16', 
'-mcpu=cortex-m4', 
'-mthumb',
'-g', 
'-specs=nano.specs',
'-gdwarf-2',
].join(' ')

LINK_LIBS = [
  '-lm',

].join(' ')

# ================================
# Project Directories
# ================================
LINKER_FILE = 'linker_script.ld'
# ================================
# INCLUDES
# ================================
# ================================
# Commands
# ================================
ARM_COMPILE_COMMAND = [
"#{ARM_GCC}",
"-c",
"#{ARM_COMPILE_OPTIONS}",
].join(" ")

# remember the \ chars, otherwise the commands will fail !!
ARM_LINK_COMMAND = [
"#{ARM_GCC}",
"#{ARM_LINK_OPTIONS}",
"-T#{LINKER_FILE}"
].join(" ")
