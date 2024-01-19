!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Sept. 22, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : generalization of module procedure
!*                               stmts, by making the MODULE keyword
!*                               optional. These statements are called
!*                               procedure statements in F2003.
!*
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  : -qlanglvl=90std
!*
!*  DESCRIPTION                : This functional test, makes sure that
!*                               when MODULE keyword is omitted and the
!*                               -qlanglvl option is set to 90/95std,
!*                               the functionality of procedure stmts
!*                               is not affected.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      module m
      implicit none

      interface gen1
         procedure s1
      end interface
      interface gen2
         procedure f1
      end interface

      contains
      subroutine s1()
        print*, "s1"
      end subroutine s1

      integer function f1(arg)
         integer :: arg
         f1 = arg
      end function f1


      end module m

      use m

      call gen1()
      print *, gen2(3)

      end
