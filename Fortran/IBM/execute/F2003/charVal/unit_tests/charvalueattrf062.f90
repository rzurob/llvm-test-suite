!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: charvalueattrf062.f
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
!*                               works as expected for procedures that have
!*                               more than 1 argument with varying lengths and
!*                               argument associations.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none

      interface
         subroutine s1(c1, c2, c3, c4)
           character*3, value :: c1
           character(1) :: c2
           character(12), value :: c3
           character(1), value :: c4
         end subroutine
      end interface

      character(3) :: x1 = 'AaA'
      character(1) :: x2, x4
      character(12) :: x3 = 'CcCCcCCcCCcC'
      x2 = 'B'
      x4 = 'D'

      call s1('AaA','B','CcCCcCCcCCcC','D')

      call s1(x1, x2, x3, x4)
      if( x1 .ne. 'AaA' ) error stop 5
      if( x2 .ne. 'u' ) error stop 6
      if( x3 .ne. 'CcCCcCCcCCcC' ) error stop 7
      if( x4 .ne. 'D' ) error stop 8

      call s1(x1, 'B', 'CcCCcCCcCCcC', x4)
      if( x1 .ne. 'AaA' ) error stop 9
      if( x4 .ne. 'D' ) error stop 10


      end

         subroutine s1(c1, c2, c3, c4)
           character*3, value :: c1
           character*1 :: c2
           character*12, value :: c3
           character*1, value :: c4

           if ( c1 .ne. 'AaA' ) error stop 1
           if ( c2 .ne. 'B' ) error stop 2
           if ( c3 .ne. 'CcCCcCCcCCcC' ) error stop 3
           if ( c4 .ne. 'D' ) error stop 4
           c1 = 'tTt'
           c2 = 'u'
           c3 = 'vVvVvVvVvVvV'
           c4 = 'w'
         end subroutine
