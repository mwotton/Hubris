require 'mkmf'

dir_config("stub")
have_header("ruby.h")
create_makefile("stub")
