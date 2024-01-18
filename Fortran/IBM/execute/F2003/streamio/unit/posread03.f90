!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: posread03.f
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
!*  DESCRIPTION                : basic read.  Some T edit descriptor
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      integer :: pos = 4
      character*10 :: c = ''

      open(unit=11, access='direct', status='replace', recl=37)
      write(11, rec=1) '1234567890abcdefghijklmnopqrstuvwxyz\n'
      close(11, status='keep')

      open(unit=11, access='stream', status='old', &
           form='formatted', pad='no')

      read(11, pos=pos, fmt='(A3)', advance='no') c
      if (c /= "456") error stop 1

      read(11, pos=10, fmt='(A10)', advance='no') c
      if (c /= "0abcdefghi") error stop 2

      read(11, fmt='(A3)', advance='no') c
      if (c /= "jkl") error stop 3

      rewind 11

      read(11, fmt='(A3)', advance='no') c
      if (c /= "123") error stop 4

      close(11, status='delete')
      end

