! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/iodiag.presh iointe01
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
!* PRIMARY FUNCTIONS TESTED     : stream I/O and internal file
!* SECONDARY FUNTIONS TESTED
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : read an internal file which is
!*                              : accessed by stream I/O method,
!*                              : it shouldn't be allowed.
!234567890123456789012345678901234567890123456789012345678901234567890
program iointe01
character(10) un

un = 'unit1'
read(un, fmt='(A3)', pos=5)
end program iointe01
