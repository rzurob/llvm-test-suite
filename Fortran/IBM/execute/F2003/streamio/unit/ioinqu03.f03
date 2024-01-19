! *********************************************************************
!* ===================================================================
!*
!
!* DATE                         : Mar. 1, 2003
!*
!* PRIMARY FUNCTIONS TESTED     : stream I/O: OPEN and INQUIRE
!* SECONDARY FUNTIONS TESTED
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : open a file by 'stream' access
!*                              : method, and inquire its' size.
!234567890123456789012345678901234567890123456789012345678901234567890
program ioinuq03

  integer position, filesize

  ! Open an exist file with holes
  open(8, access='stream', file='fileWithHole', action='write')

  ! The file is at its initial position, so position
  ! will be assigned 1.
  ! It is an existed file, the filesize is 204 bytes.
  inquire(unit=8, pos=position, size=filesize)

  close(8)
  print *, position, filesize ! prints 1, 204

end program ioinuq03
