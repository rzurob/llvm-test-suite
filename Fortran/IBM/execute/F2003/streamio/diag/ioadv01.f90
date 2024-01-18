! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/iodiag.presh ioadv01
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
!* DESCRIPTION                  : open a file by 'direct' access
!*                              : method, read it with ADVANCE
!*                              : specifier, it shouldn't be allowed.
!234567890123456789012345678901234567890123456789012345678901234567890
program ioadv01

open(unit=11, file='newFile', access='direct', recl=5)
read(11, ADVANCE='NO', REC=5)
close(11)

end program ioadv01
