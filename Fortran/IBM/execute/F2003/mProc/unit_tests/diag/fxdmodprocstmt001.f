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
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : This diagnostic test, makes sure that
!*                               if MODULE keyword is specified and
!*                               any of the procedure names is not
!*                               module procedure it is flagged.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none

      ! explicit interface for s1
      interface
         subroutine s1()
         end subroutine s1
         integer function f1()
         end function f1
      end interface

      interface generic_name1
         module procedure s1
      end interface

      interface generic_name2
         module procedure f1
      end interface

      call generic_name1()
      print*, generic_name2()
      end

      subroutine s1()
        print*, "s1"
      end subroutine s1

      integer function f1()
        f1 = 1
      end function
