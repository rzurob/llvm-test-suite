!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: fxdmodprocstmt022.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : April. 11, 2006
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
!*  DESCRIPTION                : Generic interface name is not allowed
!*                               to appear in a procedure statement ( generally ).
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      interface gen1
        subroutine s1(arg)
          integer :: arg
        end subroutine
      end interface

      interface gen2
        procedure gen1 ! should not be allowed
      end interface

      end
