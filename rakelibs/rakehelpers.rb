require 'rake'
require 'rake/clean'
require File.expand_path('./common_definitions.rb', File.dirname(__FILE__))

def headers_in(dirs)
  dir_header_patterns = []
  dirs.each {|dir| dir_header_patterns << "#{dir}*.h"}
  headers = Rake::FileList.new(dir_header_patterns)
  return headers
end

def sources_in(dirs)
  dir_source_patterns = []
  dirs.each do |dir| 
    dir_source_patterns << "#{dir}*.c"
    dir_source_patterns << "#{dir}*.S"
    dir_source_patterns << "#{dir}*.asm"
  end
  headers = Rake::FileList.new(dir_source_patterns)
  return headers
end

def abspath_of_header(headername, headerlist)
  return headerlist.detect{|f|
    f.pathmap("%n") == headername.pathmap("%n")
  }
end

def get_headers(file, headerlist)
  includes = []
  if file == nil
    raise TypeError, "The function to get the headers of a file that got 'nil'", caller
  else
    lines = File.readlines(file)
  end
  lines.each do |line|
    m = line.match(/^\s*#include\s+\"\s*(.+\.[hH])\s*\"/)
    if not m.nil?
      includes << abspath_of_header(m[1], headerlist)
    end
  end
  return includes.compact ## removing all the nils from the list, when the headers in the files are not in the given headerlist
end


def arm_include_dirs()
  dirs = Rake::FileList.new()
  dirs.include("#{SOURCE_DIR}**/**/")
  dirs.include("#{TARGET_LIBS}**/**/")
  dirs.include("Startup/**/**/")
  return dirs
end

def arm_headers()
  return headers_in(arm_include_dirs())
end

def arm_sources()
  return sources_in(arm_include_dirs())
end

def source_for_o_file(file, searched_sources)
  source = searched_sources.detect{ |f|
    f.pathmap("%n") == file.pathmap("%n")
  }
  if source
    return source
  elsif
    raise IOError, "The source to build the file \"#{file}\" could not be found. It may be a typo in the test-declarations.", caller 
  end
end

def elf_hex_bin_files(targetsym)
  targetname = targetsym.to_s
  targets = ["#{ARM_BUILD_DIR}#{targetname}.elf",
             "#{ARM_BUILD_DIR}#{targetname}.bin",
             "#{ARM_BUILD_DIR}#{targetname}.siz",
             "#{ARM_BUILD_DIR}#{targetname}.hex"]
  return targets
end

def object_files_for_elf(elf_file)
  sources = arm_sources()
  objects = Rake::FileList.new()
  sources.each do |source|
    obj_file_name = "#{ARM_BUILD_DIR}#{source.pathmap("%n")}.o"
    objects.include(obj_file_name)
  end
  return objects
end

def asm_compile_command()
  compile_command = "#{ARM_GCC}"
  compile_command += " #{ARM_ASM_OPTIONS}"
  compile_command += " #{arm_include_dirs().pathmap(' -I %p')}"
end

def compile_command_for_arm()
  compile_command = "#{ARM_GCC}"
  compile_command += " -c"
  compile_command += " #{arm_include_dirs().pathmap('-I %p')}"
  compile_command += " #{ARM_COMPILE_OPTIONS}"
  return compile_command
end
