require 'rake'
require 'rake/clean'
import './rakelibs/test_tasks.rake'
import './rakelibs/arm_tasks.rake'
require File.expand_path('./rakelibs/rakehelpers.rb', File.dirname(__FILE__))
require File.expand_path('./rakelibs/test_target_helpers.rb', File.dirname(__FILE__))

#Rake.application.options.trace_rules = true

task :default => :all

task :all => [:test, :arm]

task :flash => :arm do 
  pid = Process.spawn("~/bin/JLink_Linux_V510u_x86_64/JLinkGDBServer -device  XMC4500-1024 -if SWD -speed 4000")
  sh "arm-none-eabi-gdb -x gdbcommands"
  Process.kill("SIGHUP", pid)
end

task :arm => elf_hex_bin_files(:arm)

TESTS = {
    "pid_controller" => [
     "pid_controller",
    ],
  "fake_motion_sensor" => [
       "fake_motion_sensor",
       "motion_sensor",
   ],  
  "motion_sensor" => [
       "motion_sensor",
   ], 
   "drone_simulation" => [
      "drone_simulation",
      "drone_masses",
   ],
   "matrix_operations" =>[
      "matrix_operations",
   ],
   "physical_helpers" => [
     "physical_helpers",
     "matrix_operations",
     "math_helpers"
   ],
   "math_helpers" => [
     "math_helpers",
   ],
}

task :test => get_tests

CLOBBER.include("#{BUILD_DIR}**/**/*.o",
                "#{BUILD_DIR}**/**/*.d",
                "#{BUILD_DIR}**/**/*.c")
CLOBBER.exclude(/.*xmc.*/)
CLEAN.include(CLOBBER,
              "#{BUILD_DIR}**/**/*.elf",
              "#{BUILD_DIR}**/**/*.hex",
              "#{BUILD_DIR}**/**/*.bin",
              "#{BUILD_DIR}**/**/*.exe",
              "#{BUILD_DIR}**/**/*.map")
CLEAN.exclude(/.*xmc.*/)
