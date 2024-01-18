! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/testlang.presh iolang02
! %COMPOPTS: -qfree=f90 -qdebug=intmsg -qlanglvl=90std
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
!* TEST CASE TITLE              : iolang02.f
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
program iolang02

character(8) :: c
integer x, y

open(unit=1, file='newStreamFile', access='stream')
read(1, POS=1)
write(1, POS=1)
inquire(unit=1, stream=c, pos=x, size=y)
close(1)

end program iolang02
