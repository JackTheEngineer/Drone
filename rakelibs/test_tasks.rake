require 'rake'
require 'rake/clean'
require File.expand_path('./test_target_definitions.rb', File.dirname(__FILE__))
require File.expand_path('./test_target_helpers.rb', File.dirname(__FILE__))
import File.expand_path('./common_subtasks.rake', File.dirname(__FILE__))


rule /^test_\w+$/ => ->(f){get_test_exe(f)} do |task| 
  sh "./#{task.source}"
end

rule /#{Regexp.escape(TEST_BUILD_DIR)}test_\w+\.exe$/ => [->(testexe){deps_for_test(testexe)}] do |task|
  mkdir_p task.name.pathmap("%d")
  sh "#{LOCAL_LINK_COMMAND} -o #{task.name} #{task.prerequisites.join(" ")} -lm"
end


rule /#{Regexp.escape(TEST_BUILD_DIR)}runner_test_\w+\.o$/ => [->(runner_obj){source_of_runner(runner_obj)}] do |task|
  sh "#{compile_command_for_test(task.source, task.name)}"
end

rule /#{Regexp.escape(SOURCEGEN_DIR)}runner_test_\w+\.c$/ => ->(runnername){testname_of_runner(runnername)} do |task|
  mkdir_p task.name.pathmap("%d")
  sh "ruby #{SCRIPTS_DIR}generate_test_runner.rb -o #{task.name} -t #{RUNNER_TEMPLATE} -r #{testname_of_runner(task.name)}"
end

# Compile rule for local machine OBJ-File, which is also checking the dependencies on the .h files
rule /#{Regexp.escape(TEST_BUILD_DIR)}.+\.o$/ => [
  ->(f){source_for_o_file(f, test_sources)},
  ->(f){get_headers(source_for_o_file(f, test_sources), 
                    test_headers())}
  ] do |task|
  mkdir_p task.name.pathmap("%d")
  sh "cppcheck --force #{task.prerequisites.first}"
  sh "#{compile_command_for_test(task.prerequisites.first, task.name)}"
end

rule ".h" => ->(f){get_headers(f, test_headers())}

