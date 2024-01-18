!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: charvalueattrf059.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 26, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Validate the functionality of the VALUE
!*                               attribute when used with characters of
!*                               length other than 1. ( Feature 298120 )
!*
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Test that pass-by-value using VALUE attribute
!*                               works as expected for characters of length 1.
!*                               This tests procedures with more than 1 argument.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none

      interface
         subroutine s1(c1, c2, c3, c4)
           character*1, value :: c1, c2, c4
           character(1) c3
         end subroutine
         subroutine s2(c1, c2, c3, c4)
           character(1) :: c1, c2, c4
           character(1), value :: c3
         end subroutine
      end interface

      character(1) :: x1, x2, x3, x4
      x1 = 'A'
      x2 = 'B'
      x3 = 'C'
      x4 = 'D'

      call s1('A','B','C','D')

      call s1(x1, x2, x3, x4)
      if( x1 .ne. 'A' ) error stop 9
      if( x2 .ne. 'B' ) error stop 10
      if( x3 .ne. 'v' ) error stop 11
      if( x4 .ne. 'D' ) error stop 12

      call s1(x1, 'B', 'C', x4)
      if( x1 .ne. 'A' ) error stop 13
      if( x4 .ne. 'D' ) error stop 14


      x1 = 'A'
      x2 = 'B'
      x3 = 'C'
      x4 = 'D'

      call s2('A','B','C','D')

      call s2(x1, x2, x3, x4)
      if( x1 .ne. 't' ) error stop 15
      if( x2 .ne. 'u' ) error stop 16
      if( x3 .ne. 'C' ) error stop 17
      if( x4 .ne. 'w' ) error stop 18

      x1 = 'A'
      x4 = 'D'
      call s2(x1, 'B', 'C', x4)
      if( x1 .ne. 't' ) error stop 19
      if( x4 .ne. 'w' ) error stop 20

      x3 = 'C'
      call s2('A', 'B', x3, 'D')
      if( x3 .ne. 'C' ) error stop 21

      end

         subroutine s1(c1, c2, c3, c4)
           character*1, value :: c1, c2, c4
           character(1) c3
           if ( c1 .ne. 'A' ) error stop 1
           if ( c2 .ne. 'B' ) error stop 2
           if ( c3 .ne. 'C' ) error stop 3
           if ( c4 .ne. 'D' ) error stop 4
           c1 = 't'
           c2 = 'u'
           c3 = 'v'
           c4 = 'w'
         end subroutine

         subroutine s2(c1, c2, c3, c4)
           character(1) :: c1, c2, c4
           character(1), value :: c3
           if ( c1 .ne. 'A' ) error stop 5
           if ( c2 .ne. 'B' ) error stop 6
           if ( c3 .ne. 'C' ) error stop 7
           if ( c4 .ne. 'D' ) error stop 8
           c1 = 't'
           c2 = 'u'
           c3 = 'v'
           c4 = 'w'
         end subroutine
