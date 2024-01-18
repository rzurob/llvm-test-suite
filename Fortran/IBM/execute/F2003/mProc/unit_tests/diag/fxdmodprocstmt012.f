!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: fxdmodprocstmt012.f
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
!*  DESCRIPTION                : This diagnostic test, makes sure that
!*                               if a procedure statement appears in an
!*                               interface block that does not have a
!*                               generic interface, it is flagged.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      module m
      implicit none

      interface
        procedure s1
      end interface
      interface
        procedure f1
      end interface

      contains
      subroutine s1()
        print*, "s1"
      end subroutine s1
      integer function f1()
        f1 = 1
      end function f1
      end module

      end
