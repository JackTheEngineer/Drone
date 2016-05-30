# encoding: utf-8


## Tasks depending the ARM-Compilation Process
require 'rake'
require 'rake/clean'
require File.expand_path('./arm_target_definitions.rb', File.dirname(__FILE__))
import File.expand_path('./common_subtasks.rake', File.dirname(__FILE__))

rule ".siz" => ->(sizfile){return sizfile.ext(".elf")} do |task|
  sh "#{ARM_SIZE} --format=berkeley #{task.source}"
end

rule ".bin" => ->(binfile){return binfile.ext(".elf")} do |task|
  sh "#{ARM_OBJCOPY} -O binary #{task.source} #{task.name}"

end

rule ".hex" => [->(hexfile){ return hexfile.ext(".elf")}] do |task|
  sh "#{ARM_OBJCOPY} -O ihex #{task.source} #{task.name}"

end

rule ".elf" => ->(elf_file){object_files_for_elf(elf_file)} do |task|
  mkdir_p task.name.pathmap("%d")
  sh "#{ARM_LINK_COMMAND} -Wl,-Map,\"#{task.name.ext(".map")}\" -o #{task.name} #{task.prerequisites.join(" ")} #{LINK_LIBS}"

end

# Compile rule for an ARM OBJ-File, which is also checking the dependencies on the .h files
rule /#{ARM_BUILD_DIR}.*\.o/ => [
  ->(f){source_for_o_file(f, arm_sources())},
  ->(f){get_headers(source_for_o_file(f, arm_sources()),
                    arm_headers())}
  ] do |task|
  mkdir_p task.name.pathmap("%d")
  if task.source.pathmap("%x") == ".S"
    compile_command = asm_compile_command
  else 
    compile_command = compile_command_for_arm
  end
  sh "#{compile_command} -o \"#{task.name}\" #{task.source}"
end

# Dependency Checking of requested header files
rule ".h" => ->(f){get_headers(f, arm_headers())}
