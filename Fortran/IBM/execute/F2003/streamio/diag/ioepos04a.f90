! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/iodiag.presh ioepos04a
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
!* XL Fortran Test Case                         INBM INTERNAL USE ONLY
!* ===================================================================
!*
!* TEST CASE TITLE              : ioepos04a.f
!
!* PROGRAMMER                   : Helen Li
!* DATE                         : Oct. 7, 2002
!* ORIGIN                       : AIX Complier Development
!*                              : IBM Software Solutions Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED     : stream I/O: READ with POS specifier
!* SECONDARY FUNTIONS TESTED
!*
!* DRIVER STANZA                : xlf95
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : open a file by 'stream' access
!*                              : method, write it with invalid POS
!*                              : specifier, it shouldn't be allowed. 
!*                              :
!234567890123456789012345678901234567890123456789012345678901234567890
program ioepos04a

open(unit=11, file='newStreamFile', access='stream')
write(11, FMT='(A3)', ADVANCE='NO', POS=-5) 
close(11)

end program ioepos04a
