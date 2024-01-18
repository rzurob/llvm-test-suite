! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/iounit.presh ioopin04
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
!*                              : 'stream' access method is passed
!*                              : by parameter
!234567890123456789012345678901234567890123456789012345678901234567890
program ioopin04
character(6) :: STR = 'stream'
call sub(STR)
end program ioopin04

subroutine sub(str)
character(6) str
character(3) :: c = ''
integer x, y

open(unit=1, file='newStreamFile', access = STR)
inquire(unit=1, stream=c, pos=x, size=y)
print *, c
print *, x
print *, y
close(1)

end subroutine
