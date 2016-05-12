# Directories and constants
require 'rake'
require 'rake/clean'



UNITY_ROOT = "vendor/Unity/"
BUILD_DIR = 'build/'
ARM_BUILD_DIR = BUILD_DIR + 'arm/'


CLANG_CHECKER = './vendor/clang_checker/bin/scan-build'
SOURCE_DIR = "src/"
TEST_DIR = 'test/'
SUPPORT_DIR = TEST_DIR + 'support/'
TEST_BUILD_DIR = BUILD_DIR + 'test/'
SOURCEGEN_DIR = BUILD_DIR + 'sourcegen/'
SCRIPTS_DIR = "rakelibs/"
RUNNER_TEMPLATE = "#{SCRIPTS_DIR}runner_template.c"
TARGET_LIBS = "XMC_Libs/"
XMC_OBJS = Rake::FileList.new("XMC_objects/*.o")

UNITY_SOURCE_DIRS = Rake::FileList.new("#{UNITY_ROOT}src/", "#{UNITY_ROOT}extras/fixture/src/")
UNITY_SOURCES = Rake::FileList.new("#{UNITY_ROOT}src/unity.c", "#{UNITY_ROOT}extras/fixture/src/unity_fixture.c")
