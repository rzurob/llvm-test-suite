! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/iounit.presh ioopin02
! %COMPOPTS: -qfree=f90
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
!* DATE                         : Mar. 1, 2003
!* ORIGIN                       : AIX Complier Development
!*
!* PRIMARY FUNCTIONS TESTED     : stream I/O: OPEN and INQUIRE
!* SECONDARY FUNTIONS TESTED
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : open a file by 'stream' access
!*                              : method and inquire it.
!*                              : 'stream' access method is defined
!*                              : as variable.
!234567890123456789012345678901234567890123456789012345678901234567890
program ioopin02

character(3) :: c = ''
character(6) :: STR = 'stream'
integer x, y, z

open(unit=1, file='newStreamFile', access = STR)
inquire(unit=1, stream=c, pos=x, size=y)
print *, c
print *, x
print *, y
close(1)

end program ioopin02
