!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: posread01.f
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
!*  DESCRIPTION                : basic read.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      integer :: pos, size
      character*10 :: c = ''

      open(unit=11, access='direct', status='replace', recl=37)
      write(11, rec=1) '1234567890abcdefghijklmnopqrstuvwxyz\n'
      close(11, status='keep')


      open(unit=11, access='stream', status='old', form='formatted')

      ! Initial file position is 1
      inquire(11,pos=pos)
      if (pos /= 1) error stop 1

      ! Read using pos
      pos = 4
      read(11,pos=pos, fmt='(A3)', advance='no') c
      if (c /= "456       ") error stop 2
      inquire(11,pos=pos)
      if (pos /= 7) error stop 3

      ! Read using another pos
      read(11,pos=10, fmt='(A10)', advance='no') c
      if (c /= "0abcdefghi") error stop 4
      inquire(11,pos=pos)
      if (pos /= 20) error stop 5

      close(11, status='keep')

      ! position=append should open the file and position it at the very end.
      open(unit=11, access='stream', status='old', position='append')
      inquire(11, pos=pos, size=size)
      if (pos /= 38) error stop 6
      if (size /= 37) error stop 7

      ! Try reading with an empty input list.  Nothing should happen.
      read(11)


      close(11, status='delete')
      end
