# encoding: utf-8

require 'rake'
require 'rake/clean'

begin
  require File.expand_path('test/test_defines.rb', File.dirname(__FILE__))
rescue LoadError
  $TESTS = {}
end

# Rake.application.options.trace_rules = true

##
## If you have your rake-buildsystem in 
## the same directory as your falcon Project,
## then, this default is correct
##
## Otherwise,
## change this path to where you have put your rakelibs 
##

RAKELIBS_LOCATION = File.join(File.dirname(__FILE__), "../rake-buildsystem/")
require File.join(RAKELIBS_LOCATION, "rakefile.rb")

task :flash => :arm do 
  pid = Process.spawn("~/bin/JLink_Linux_V616c_x86_64/JLinkGDBServer -device XMC4500-F100x1024 -if SWD -speed 1000")
  sh "~/bin/gcc-arm-none-eabi-4_9-2014q4/bin/arm-none-eabi-gdb -x gdbcommands"
  Process.kill("SIGHUP", pid)
end

task :run_simulation => "simulation_graphics/draw_data.txt" do |task|
  sh "gnuplot simulation_graphics/gnuplotcommands"
end

task "simulation_graphics/draw_data.txt" => :test_simulation

CLEAN.exclude(/.*xmc.*/)
