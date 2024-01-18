! *********************************************************************
!* ===================================================================
!*
!
!* DATE                         : Oct. 7, 2002
!*
!* PRIMARY FUNCTIONS TESTED     : stream I/O and internal file
!* SECONDARY FUNTIONS TESTED
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : read an internal file which is
!*                              : accessed by stream I/O method,
!*                              : it shouldn't be allowed.
!234567890123456789012345678901234567890123456789012345678901234567890
module mod
character(10) un
contains
subroutine sub
un = 'unit1'
read(un, fmt='(A3)', pos=5)
end subroutine
end module

program iointe02
use mod
call sub
end program iointe02
