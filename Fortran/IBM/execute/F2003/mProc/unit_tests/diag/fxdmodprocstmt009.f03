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
!*                               if MODULE is not specified and the identifier
!*                               refers to an internal procedure,
!*                               it is flagged.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      subroutine s1()
      implicit none

      interface gen1
        procedure intS1
      end interface gen1
      interface gen2
        procedure intF1
      end interface gen2

      contains
      subroutine intS1(arg)
         integer :: arg
         print*, "intS1 ", arg
      end subroutine intS1
      integer function intF1()
        intF1 = 1
      end function

      end subroutine s1


      end
