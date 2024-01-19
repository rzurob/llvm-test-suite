! *********************************************************************
!* ===================================================================
!*
!
!* DATE                         : Oct. 7, 2002
!*
!* PRIMARY FUNCTIONS TESTED     : stream I/O: READ with POS specifier
!* SECONDARY FUNTIONS TESTED
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : open a file by 'stream' access
!*                              : method, write it with invalid POS
!*                              : specifier, it shouldn't be allowed.
!234567890123456789012345678901234567890123456789012345678901234567890
program ioepos03

open(unit=11, file='newStreamFile', access='stream')
read(11, FMT='(A3)', ADVANCE='NO', POS=0)
close(11)

end program ioepos03
