!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: charvalueattrd010.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 24, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : providing support for character dummy
!*                               arguments with length other than 1 to
!*                               have the VALUE attribute (Feature 298120).
!*
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  : -qdebug=intmsg
!*  REQUIRED RUNTIME OPTIONS   :
!*
!*  DESCRIPTION                : This diagnostic test, makes sure that
!*                               characters of runtime length with VALUE
!*                               attribute get flagged. This tests the
!*                               VALUE stmt before declaration.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none

      interface
         subroutine s1(a, t)
           integer :: t
           value :: a
           character(t) :: a
         end subroutine s1
      end interface

      end

      subroutine s1(arg, i)
        value :: arg
        integer :: i
        character(i) :: arg
      end subroutine s1
