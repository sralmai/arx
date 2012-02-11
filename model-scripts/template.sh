#!/bin/sh
set -e -u
unset x y z
while [ $# -gt 0 ]
do
  case "$1" in
    //) : ;; # Stop here. ARGV will be passed as is below.
    *) : ;; # Process the arguments to set control variables.
  esac
  shift
done
# Set control variables.
: ${x:=...}
: ${y:=...}
: ${z:=...}
work() {
  # The default command is used if there are not positional parameters.
  [ $# -gt 0 ] || set -- run fast
  # The wrapper is called with "$@", whatever it is after all this mangling.
  screend ./lib cd_ "$dir" tmpx_trap "$@"
}
setup_of_all_kinds() {

}
work "$@"

