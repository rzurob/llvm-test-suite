! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/iounit.presh ioopin05
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
!* XL Fortran Test Case                         INBM INTERNAL USE ONLY
!* ===================================================================
!*
!* TEST CASE TITLE              : ioopin05.f
!
!* PROGRAMMER                   : Helen Li
!* DATE                         : Mar. 1, 2003
!* ORIGIN                       : AIX Complier Development
!*                              : IBM Software Solutions Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED     : stream I/O: OPEN and INQUIRE
!* SECONDARY FUNTIONS TESTED
!*
!* DRIVER STANZA                : xlf95
!* REQUIRED COMPILER OPTIONS    : 
!*
!* DESCRIPTION                  : open a file by 'stream' access
!*                              : method and inquire it.
!*                              : 'stream' access method is defined
!*                              : in a module.
!*                              : 
!234567890123456789012345678901234567890123456789012345678901234567890
module mod
  character(6) :: STR = 'stream'
end module

program ioopin05
  use mod
  call sub

  contains
  subroutine sub
    character(3) :: c = ''
    integer x, y

    open(unit=1, file='newStreamFile', access = STR)
    inquire(unit=1, stream=c, pos=x, size=y)
    print *, c
    print *, x
    print *, y

    close(1)

  end subroutine

end program ioopin05
