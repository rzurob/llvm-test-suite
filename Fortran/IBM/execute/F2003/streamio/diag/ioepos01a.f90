! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/iodiag.presh ioepos01a
! %COMPOPTS: -qfree=f90 -qdebug=intmsg
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!* ===================================================================
!*
!
!* DATE                         : Oct. 7, 2002
!* ORIGIN                       : AIX Complier Development
!*
!* PRIMARY FUNCTIONS TESTED     : stream I/O: READ with POS specifier
!* SECONDARY FUNTIONS TESTED
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : open a file by 'stream' access
!*                              : method, read it with undefined POS
!*                              : specifier, it shouldn't be allowed.
!234567890123456789012345678901234567890123456789012345678901234567890
program ioepos01a
open(unit=11, file='newStreamFile', access='stream')
write(11, FMT='(A3)', ADVANCE='NO', POS=x)
close(11)
end program ioepos01a
