! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/iounit.presh ioinqu01
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
!* TEST CASE TITLE              : ioinqu01.f
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
!*                              :
!234567890123456789012345678901234567890123456789012345678901234567890
program ioinqu01

  integer position, filesize, iol
  character(7) :: C = 'abcdefg'
 
  ! Create a new file
  open(7, access='stream', file='newStreamFile', status='new')

  ! The file is at its initial position, so position
  ! will be assigned 1.
  ! Nothing was written to the new file, so filesize
  ! is zero bytes.
  inquire(unit=7, pos=position, size=filesize)

  ! iol will be assigned 7 because character variable C
  ! has seven characters in it.
  inquire(iolength=iol) C

  close(7)

  print *, iol, position, filesize ! prints 7, 1, 0

end program ioinqu01
