!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: fxdmodprocstmt011.f
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
!*  REQUIRED COMPILER OPTIONS  : -qlanglvl=90/95/pure/std
!*
!*  DESCRIPTION                : This diagnostic test, makes sure that
!*                               if MODULE keyword is omitted and -qlanglvl
!*                               is set to 90std/pure or 95std/pure, an
!*                               appropriate message is given.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      module m
      implicit none

      interface gen
        procedure s1
      end interface

      contains
      subroutine s1()
         print*, "s1"
      end subroutine s1

      end module m

      use m

      call gen()

      end
