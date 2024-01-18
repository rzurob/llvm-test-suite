!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: backspace01.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************

!*  ===================================================================
!*
!*  DATE                       : April 2003
!*  ORIGIN                     : AIX Compiler Development,
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
!*  DESCRIPTION                : Backspace Statement
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none
      character*3 :: c3
      character*38 :: c38
      integer pos, i

      ! Create a file with 10 lines of:
      ! 1234567890abcdefghijklmnopqrstuvwxyz
      open(11, access='stream', status='scratch', form='formatted')
      do i = 1, 10
        write(11, '(A)') '1234567890abcdefghijklmnopqrstuvwxyz'
      end do
      rewind(11)


      ! We're at the beginning of the second record.  Backspace should
      ! return us to the beginning of the first record.

      c38 = ''
      read(11, fmt='(A)') c38
      if (c38 /= "1234567890abcdefghijklmnopqrstuvwxyz") error stop 1
      inquire(11, pos=pos)
      ! File position before backspace
      if (pos /= 38) error stop 2
      backspace 11
      inquire(11, pos=pos)
      ! File position after backspace
      if (pos /= 1) error stop 3

      rewind(11)


      ! We're in the middle of the second record.  Backspace should
      ! return us to the beginning of the second record.

      c3 = ''
      read(11, pos=38, fmt='(A3)', advance='no') c3
      if (c3 /= "123") error stop 4
      inquire(11, pos=pos)
      ! File position before backspace
      if (pos /= 41) error stop 5
      backspace 11
      inquire(11, pos=pos)
      ! File position after backspace
      if (pos /= 38) error stop 6

      rewind(11)


      ! We're in the middle of the second record (via the T edit descriptor)
      ! Backspace should return us to the beginning of the second record.

      c38 = ''
      pos = 0
      read(11, fmt='(A)') c38
      if (c38 /= "1234567890abcdefghijklmnopqrstuvwxyz") error stop 7
      read(11, fmt='(T5)', advance='no')
      inquire(11, pos=pos)
      ! File position before backspace
      if (pos /= 42) error stop 8
      backspace 11
      inquire(11, pos=pos)
      ! File position after backspace
      if (pos /= 38) error stop 9

      rewind(11)


      ! We're at the beginning of the third record (via the T edit descriptor
      ! and advance=yes).  Backspace should return us to the beginning of the
      ! second record.
      c38 = ''
      pos = 0
      read(11, fmt='(A37)') c38
      if (c38 /= "1234567890abcdefghijklmnopqrstuvwxyz  ") error stop 10
      read(11, fmt='(TR5,A3)') c38   ! advance=yes, pad=yes
      if (c38 /= "678                                   ") error stop 11
      inquire(11, pos=pos)
      ! File position before backspace
      if (pos /= 75) error stop 12
      backspace 11
      inquire(11, pos=pos)
      ! File position after backspace
      if (pos /= 38) error stop 13

      end
