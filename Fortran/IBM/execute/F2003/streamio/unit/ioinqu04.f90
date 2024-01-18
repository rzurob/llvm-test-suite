! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/iounit.presh ioinqu04
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
!* TEST CASE TITLE              : ioinqu04.f
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
!*                              : method, and inquire its' size.
!*                              : All happened in subroutine.
!*                              :
!234567890123456789012345678901234567890123456789012345678901234567890
program ioinqu04

  integer position, filesize

  call sub
  print *, position, filesize ! prints 1, 165

  contains

  subroutine sub
 
  ! Open an exist file
  open(7, access='stream', file='existStreamFile', action='read')

  ! The file is at its initial position, so position
  ! will be assigned 1.
  ! It is an existed file, the filesize is 165 bytes.
  inquire(unit=7, pos=position, size=filesize)

  close(7)
  end subroutine

end program ioinqu04
