! *********************************************************************
!* ===================================================================
!*
!
!* DATE                         : Oct. 7, 2002
!*
!* PRIMARY FUNCTIONS TESTED     : stream I/O: READ with POS and REC specifier
!* SECONDARY FUNTIONS TESTED
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : open a file by 'stream' access
!*                              : method, read it with POS and REC
!*                              : specifier, it shouldn't be allowed.
!234567890123456789012345678901234567890123456789012345678901234567890
module mod
integer x, y
contains
subroutine sub
open(unit=11, file='newStreamFile', access='stream')
read(11, FMT='(A3)', ADVANCE='NO', POS=x, REC=y, EOR=100)
close(11)

100 PRINT *, 'end of unit reached'
end subroutine
end module
program iopandr02
use mod
call sub
end program iopandr02