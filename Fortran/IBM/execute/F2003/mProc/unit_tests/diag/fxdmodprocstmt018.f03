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
!*  DESCRIPTION                : This test case makes sure that if a
!*                               procedure statement ( not a proc
!*                               declaration ) appears outside of an
!*                               interface block, it is flagged.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none

      interface
         subroutine s1()
         end subroutine s1
      end interface

      interface
         integer function f1()
         end function f1
      end interface

      procedure s1
      procedure f1
      procedure s1, f1

      module procedure s1
      module procedure f1
      module procedure s1, f1


      end

      subroutine s1()
         print*, "s1"
      end subroutine s1

      integer function f1()
         f1 = 1
      end function f1
