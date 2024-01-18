!**********************************************************************

!*  ===================================================================
!*
!*  DATE                       : April 2003
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : basic read.  Some T edit descriptor
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      integer :: pos = 0
      character*10 :: c = ''
      character*10 :: d = ''
      character*4097 :: e = ''

      ! Create a file using sequential IO
      ! The whole file is made up of one huge record.
      open(unit=11, status='replace')
      do i = 1, 400
        write(11, '(A,$)') '1234567890abcdefghijklmnopqrstuvwxyzN'
      end do
      close(11, status='keep')

      ! Read via formatted stream IO
      open(unit=11, access='stream', form='formatted', status='old')

      ! Use T edit descriptor
      read(11, pos=4, fmt='(A3,T1,A1)', advance='no') c, d
      if (c /= '456       ') error stop 1
      if (d /= '4         ') error stop 2
      inquire(11,pos=pos)
      if (pos /= 5) error stop 3

      ! Span buffers.  Default buffer size = block size = 4096 bytes
      read(11,pos=1, fmt='(A4097,T1,A1)', advance='no') e, d
      if (e(1:3) /= '123') error stop 4
      if (e(4090:4097) /= 'jklmnopq') error stop 5
      if (d /= '1') error stop 6
      inquire(11,pos=pos)
      if (pos /= 2) error stop 7
      end
