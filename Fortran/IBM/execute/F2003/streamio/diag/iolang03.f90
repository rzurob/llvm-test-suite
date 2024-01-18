! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/testlang.presh iolang03
! %COMPOPTS: -qfree=f90 -qdebug=intmsg -qlanglvl=77std
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
!* TEST CASE TITLE              : iolang03.f
!
!* PROGRAMMER                   : Helen Li
!* DATE                         : Oct. 7, 2002
!* ORIGIN                       : AIX Complier Development
!*                              : IBM Software Solutions Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED     : stream I/O: OPEN, READ/WRITE and INQUIRE
!* SECONDARY FUNTIONS TESTED
!*
!* DRIVER STANZA                : xlf95
!* REQUIRED COMPILER OPTIONS    : 
!*
!* DESCRIPTION                  : open a file by 'stream' access,
!*                              : read/write it with POS specifier, 
!*                              : and inquire it with POS, SIZE and STREAM
!*                              : specifiers, it will be flagged with langage
!*				: level options.
!234567890123456789012345678901234567890123456789012345678901234567890
PROGRAM LANG03
CHARACTER(8) C
INTEGER X, Y

OPEN(UNIT=1, FILE='newFile', ACCESS='stream')
READ(1, POS=1)
WRITE(1, POS=1)
INQUIRE(UNIT=1, STREAM=C, POS=X, SIZE=Y)
CLOSE(1)

END
