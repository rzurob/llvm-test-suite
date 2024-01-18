! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/iounit.presh ioinqu02
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
!*                              : method, and inquire its' size.
!234567890123456789012345678901234567890123456789012345678901234567890
program ioinqu02

  integer position, filesize

  ! Open an exist file
  open(7, access='stream', file='existStreamFile', action='read')

  ! The file is at its initial position, so position
  ! will be assigned 1.
  ! It is an existed file, the filesize is 165 bytes.
  inquire(unit=7, pos=position, size=filesize)

  close(7)
  print *, position, filesize ! prints 1, 165

end program ioinqu02
