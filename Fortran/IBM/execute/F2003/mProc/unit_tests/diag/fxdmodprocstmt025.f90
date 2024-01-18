!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: fxdmodprocstmt025.f
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
!*  DESCRIPTION                : A procedure appearing in the procedure dcl
!*                               stmt should be allowed in proc stmt if
!*                               it has an accessible explicit iface. There
!*                               should be no errors issued if the proc dcl
!*                               appears after the interface block.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

! the following are prohibited
      procedure(integer) :: xx

      interface gen1
        procedure xx
      end interface

      procedure() :: yy

      interface gen2
        procedure yy
      end interface


! the following are prohibited
      interface gen11
        procedure xxx
      end interface

      procedure(real) :: xxx

      interface gen22
        procedure yyy
      end interface

      procedure() :: yyy

! the following are fine:
      interface gen111
        procedure xxxx
      end interface

      procedure(sub1) :: xxxx

      procedure(sub1) :: yyyy

      interface gen222
        procedure yyyy
      end interface

      contains
      subroutine sub1()
      end subroutine

      end
