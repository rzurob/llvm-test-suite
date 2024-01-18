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
!*  DESCRIPTION                : If an interface block appears within
!*                               the very procedure that is being listed
!*                               in a procedure statement, inside that
!*                               block, then the procedure must be
!*                               recursive and if it is a function it
!*                               must have RESULT specified. If these
!*                               conditions are not met, an error message
!*                               should be given. This tests the
!*                               diagnostic message when MODULE is specified
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module m
      interface gens
        module procedure s1
      end interface

      interface genf
        module procedure f1
      end interface genf

      contains
      integer function f1()
      interface i1
        module procedure f1
      end interface i1
        f1 = 1
      end function
      subroutine s1()
      interface i2
        module procedure s1
      end interface i2
        print*,"s1"
      end subroutine s1
end module m

integer function f2()
      interface gi2
        module procedure f2
      end interface gi2
      f2 = 2
end function f2

subroutine s2()
      interface gi2s
        module procedure s2
      end interface gi2s
      print *, "s2"
end subroutine s2
