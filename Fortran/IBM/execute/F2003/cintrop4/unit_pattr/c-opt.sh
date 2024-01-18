#!/bin/sh
### c-opt -- select Fortran options and map them to C options.
#
# Some Fortran compilation options must also be used when
# compiling C objects that will be linked with the Fortran objects.

for j do
  case $j in
    -q64|-qnoopt|-O|-O2|-O3|-O4|-O5|-qipa|-qsmp*)  cF="$cF $j" ;;
    -qextchk|-qnolm|-qpdf*)  cF="$cF $j" ;;

    # Some must be converted to their xlc equivalents:
    -O0)  cF="$cF -qnoopt" ;;
  esac
done

echo $cF
