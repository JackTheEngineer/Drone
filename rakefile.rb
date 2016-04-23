require 'rake'
require 'rake/clean'
import './rakelibs/test_tasks.rake'
import './rakelibs/arm_tasks.rake'
require File.expand_path('./rakelibs/rakehelpers.rb', File.dirname(__FILE__))
require File.expand_path('./rakelibs/test_target_helpers.rb', File.dirname(__FILE__))

#Rake.application.options.trace_rules = true

task :default => :all

task :all => :test

task :flash => :arm do 
  pid = Process.spawn("/home/jakov/bin/JLink_Linux_V510u_x86_64/JLinkGDBServer -device  XMC4500-1024 -if SWD -speed 4000")
  sh "arm-none-eabi-gdb -x gdbcommands"
  Process.kill("SIGHUP", pid)
end

task :arm => elf_hex_bin_files(:arm)

task :default => :test

task :test => get_tests
