! *********************************************************************
!* ===================================================================
!*
!
!* DATE                         : Oct. 7, 2002
!*
!* PRIMARY FUNCTIONS TESTED     : stream I/O: OPEN with RECL specifier
!* SECONDARY FUNTIONS TESTED
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : open a file by 'stream' access
!*                              : method with RECL specifier,
!*                              : it shouldn't be allowed.
!234567890123456789012345678901234567890123456789012345678901234567890
program iorecl01

open(unit=11, file='newStreamFile', access='stream', recl=5)
close(11)

end program iorecl01
