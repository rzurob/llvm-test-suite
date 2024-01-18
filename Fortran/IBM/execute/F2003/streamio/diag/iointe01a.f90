! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/iodiag.presh iointe01a
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
!* TEST CASE TITLE              : iointe01a.f
!
!* PROGRAMMER                   : Helen Li
!* DATE                         : Oct. 7, 2002
!* ORIGIN                       : AIX Complier Development
!*                              : IBM Software Solutions Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED     : stream I/O and internal file
!* SECONDARY FUNTIONS TESTED
!*
!* DRIVER STANZA                : xlf95
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : read an internal file which is 
!*                              : accessed by stream I/O method,
!*                              : it shouldn't be allowed. 
!*                              :
!234567890123456789012345678901234567890123456789012345678901234567890
program iointe01a
character(10) un

un = 'unit1'
write(un, fmt='(A3)', pos=5)
end program iointe01a
