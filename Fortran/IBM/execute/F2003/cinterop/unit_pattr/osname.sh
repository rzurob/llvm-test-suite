#!/bin/sh
### osname -- return a short string representing the Operating System
### of the host where osname was invoked.
#
# Used to construct filenames of test verification files, when different
# verification files are needed for different OSs.

## whichLx crashes on AIX.  Work around it.
#?? Ideally, whichLx shouldn't even exist!  isAIX should be handling it all.
#
test -f /etc/issue  && linuxDist=`whichLx`

case `isAIX`.$linuxDist in
   0.*) echo  aix ;;  # AIX
   1.1) echo rhel ;;  # RedHat Enterprise Linux
   1.0) echo suse ;;  # SuSE Linux
   2.*) echo  mac ;;  # MacOSX
   *) echo unknownOS ;;  # facilitate debugging: user will eventually get an
   #                     # error like `file unknownOS-<testcase>.vf not found'.
esac
