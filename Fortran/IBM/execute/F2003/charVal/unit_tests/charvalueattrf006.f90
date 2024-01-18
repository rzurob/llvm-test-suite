!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 25, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Validate the functionality of the VALUE
!*                               attribute when used with characters of
!*                               length other than 1. ( Feature 298120 )
!*
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Test LEN and INDEX intrinsics when characters with
!*                               VALUE attr are passed to them. Test character of
!*                               length 3.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none

      interface
         subroutine s1(c)
           character(3), value :: c
         end subroutine
      end interface

      character(3) x

      x = 'AbC'

      call s1(x)

      if( x .ne. 'AbC' ) error stop 5

      end


      subroutine s1(arg)
        character(3), value :: arg
        integer :: ilen, ix

        ilen = len(arg)
        if ( ilen .ne. 3 ) error stop 1

        ix = index(arg, 'b')
        if ( ix .ne. 2 ) error stop 2

        if ( index(arg, 'C', back=.true.) .ne. 3 ) error stop 3

        if ( index(arg, 'a') .ne. 0 ) error stop 4


      end subroutine s1

