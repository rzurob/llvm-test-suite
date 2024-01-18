!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: fxdmodprocstmt013.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Sept. 22, 2005
!*  ORIGIN                     : AIX Compiler Development,
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
!*                               should be given.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      subroutine s1()
      interface gen1
         procedure s1  ! this should be flagged because s1 is not recursive
      end interface
      end subroutine s1

      recursive subroutine s2()
      interface gen2
         procedure s2  ! this should be fine because s2 is recursive
      end interface
      end subroutine s2

      recursive integer function f1()
      interface gen3
         procedure f1  ! this should be flagged because f1 does not have the RESULT specified
      end interface
      f1 = 0
      end function f1

      recursive function f2() result(res1)
      integer :: res1
      interface gen4
         procedure f2  ! this should be fine because f2 is recursive and has RESULT specified
      end interface
      res1 = 2
      end function f2
