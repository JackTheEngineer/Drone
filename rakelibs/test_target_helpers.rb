require File.expand_path('./common_definitions.rb', File.dirname(__FILE__))
require File.expand_path('./test_target_definitions.rb', File.dirname(__FILE__))
require File.expand_path('./rakehelpers.rb', File.dirname(__FILE__))
require 'rake'
require 'rake/clean'


def test_include_dirs
  dirs = Rake::FileList.new("#{TEST_DIR}**/**/")
  dirs.include(UNITY_SOURCE_DIRS)
  dirs.include("#{SOURCE_DIR}**/**/")
  return dirs
end

def test_headers
  return headers_in(test_include_dirs)
  
end

def test_sources
  return sources_in(test_include_dirs)
end

def deps_for_test(testnameexe)
    testname = testnameexe.match(/test_(\w+)\.exe$/)[1]
    source_deps = TESTS[testname]
    source_o_files = []
    if source_deps
      source_deps.each {|dep| source_o_files << "#{TEST_BUILD_DIR}#{dep.ext('.o')}"}
    end
    runner = "#{TEST_BUILD_DIR}runner_test_#{testname}.o"
    unity_o_files = UNITY_SOURCES.pathmap("#{TEST_BUILD_DIR}%n.o")
    test_obj = "#{TEST_BUILD_DIR}test_#{testname}.o"
    ofiles = Rake::FileList.new(test_obj, unity_o_files, source_o_files, runner)
    return ofiles
end

def source_of_runner(runner_o)
  return runner_o.pathmap("#{SOURCEGEN_DIR}%n.c")
end

def testname_of_runner(runner_o)
  # So it still works if you hand him a c file
  testname = runner_o.match(/runner_test_(\w+)\.[oc]$/)[1]
  return "#{TEST_DIR}test_#{testname}.c"
end

def get_test_exe(test_name)
  return "#{TEST_BUILD_DIR}#{test_name}.exe"
end

def get_tests()
  tests = []
  TESTS.keys.each {|testname| tests << "test_#{testname}"}
  return tests
end

def compile_command_for_test(c_source_file, o_file_name)
  compile_command = "#{LOCAL_GCC}"
  compile_command += " -c"
  compile_command += " -o #{o_file_name}"
  compile_command += " #{LOCAL_COMPILER_OPTIONS}"
  compile_command += " #{test_include_dirs().pathmap( "-I %p")}"
  compile_command += " -I- "
  compile_command += " #{c_source_file}"
  compile_command += " #{LOCAL_GCC_COMPILE_LIBS}"
  return compile_command
end

