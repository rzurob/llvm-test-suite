!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 26, 2006
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
!*                               works as expected for characters of length 3.
!*                               This tests procedures with more than 1 argument.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none

      interface
         subroutine s1(c1, c2, c3, c4)
           character*3, value :: c1, c2, c4
           character(3) c3
         end subroutine
         subroutine s2(c1, c2, c3, c4)
           character(3) :: c1, c2, c4
           character(3), value :: c3
         end subroutine
      end interface

      character(3) :: x1, x2, x3, x4
      x1 = 'AaA'
      x2 = 'BbB'
      x3 = 'CcC'
      x4 = 'DdD'

      call s1('AaA','BbB','CcC','DdD')

      call s1(x1, x2, x3, x4)
      if( x1 .ne. 'AaA' ) error stop 9
      if( x2 .ne. 'BbB' ) error stop 10
      if( x3 .ne. 'vVv' ) error stop 11
      if( x4 .ne. 'DdD' ) error stop 12

      call s1(x1, 'BbB', 'CcC', x4)
      if( x1 .ne. 'AaA' ) error stop 13
      if( x4 .ne. 'DdD' ) error stop 14


      x1 = 'AaA'
      x2 = 'BbB'
      x3 = 'CcC'
      x4 = 'DdD'

      call s2('AaA','BbB','CcC','DdD')

      call s2(x1, x2, x3, x4)
      if( x1 .ne. 'tTt' ) error stop 15
      if( x2 .ne. 'uUu' ) error stop 16
      if( x3 .ne. 'CcC' ) error stop 17
      if( x4 .ne. 'wWw' ) error stop 18

      x1 = 'AaA'
      x4 = 'DdD'
      call s2(x1, 'BbB', 'CcC', x4)
      if( x1 .ne. 'tTt' ) error stop 19
      if( x4 .ne. 'wWw' ) error stop 20

      x3 = 'CcC'
      call s2('AaA', 'BbB', x3, 'DdD')
      if( x3 .ne. 'CcC' ) error stop 21

      end

         subroutine s1(c1, c2, c3, c4)
           character*3, value :: c1, c2, c4
           character(3) c3
           if ( c1 .ne. 'AaA' ) error stop 1
           if ( c2 .ne. 'BbB' ) error stop 2
           if ( c3 .ne. 'CcC' ) error stop 3
           if ( c4 .ne. 'DdD' ) error stop 4
           c1 = 'tTt'
           c2 = 'uUu'
           c3 = 'vVv'
           c4 = 'wWw'
         end subroutine

         subroutine s2(c1, c2, c3, c4)
           character(3) :: c1, c2, c4
           character(3), value :: c3
           if ( c1 .ne. 'AaA' ) error stop 5
           if ( c2 .ne. 'BbB' ) error stop 6
           if ( c3 .ne. 'CcC' ) error stop 7
           if ( c4 .ne. 'DdD' ) error stop 8
           c1 = 'tTt'
           c2 = 'uUu'
           c3 = 'vVv'
           c4 = 'wWw'
         end subroutine
