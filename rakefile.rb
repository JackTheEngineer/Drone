# encoding: utf-8

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
  "test_pid_controller" => [
    "pid_controller",
  ],
  "test_motion_sensor" => [
    "motion_sensor",
    "vector_operations",
    "vector_tester",
  ], 
  "test_drone_physics" => [
    "drone_physics",
    "drone_masses",
    "propeller",
    "vector_operations",
    "vector_tester",
    "matrix_operations",
  ],
  "test_matrix_operations" =>[
    "matrix_operations",
    "vector_operations",
  ],
  "test_physical_helpers" => [
    "physical_helpers",
    "matrix_operations",
    "vector_operations"
  ],
  "test_vector_operations" => [
    "vector_operations",
    "vector_tester"
  ],
  "test_propeller" => [
    "propeller",
  ],
  "test_simulation_connection" => [
    "simulation",
    "drone_physics",
    "vector_operations",
    "fake_motors",
  ],
  "test_fake_motors"=>[
    "fake_motors",
    "simulation",
    "vector_operations",
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
