! *********************************************************************
!* ===================================================================
!*
!
!* DATE                         : Mar. 1, 2003
!*
!* PRIMARY FUNCTIONS TESTED     : stream I/O: OPEN with RECL specifier
!* SECONDARY FUNTIONS TESTED
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : open a file in module by 'stream'
!*                              : access method with RECL specifier,
!*                              : it shouldn't be allowed.
!234567890123456789012345678901234567890123456789012345678901234567890
module mod
contains
subroutine sub
open(unit=11, file='newStreamFile', access='stream', recl=5)
close(11)
end subroutine
end module

program iorecl02
use mod
call sub
end program iorecl02