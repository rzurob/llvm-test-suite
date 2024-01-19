!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : April. 11, 2006
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
!*  DESCRIPTION                : Generic interface name is not allowed
!*                               to appear in a module procedure statement
!*                               ( generally ).
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m

      interface gen2
        module procedure s1
      end interface

      interface gen1
        module procedure gen2
      end interface

      contains
      subroutine s1()
        print *, "s1"
      end subroutine

end module

program main

use m

end program
