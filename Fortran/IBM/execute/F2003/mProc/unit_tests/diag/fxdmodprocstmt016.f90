!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: fxdmodprocstmt016.f
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
!*  DESCRIPTION                : If a generic iface contains a proc stmt
!*                               that refers to a func, and one that
!*                               refers to a subroutine it should be
!*                               flagged with an error message.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      module m
      implicit none

      interface gen
        module procedure s1, f1
        procedure s1, f1
      end interface gen

      contains
      subroutine s1()
        print*, "s1"
      end subroutine s1
      integer function f1()
        f1 = 1
      end function

      end
