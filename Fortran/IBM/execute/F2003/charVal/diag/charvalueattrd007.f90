!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: charvalueattrd007.f
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
!*                               characters of assumed length with VALUE
!*                               attribute get flagged. This tests the
!*                               VALUE statement before the declaration.
!*                               Testing "character*(*)" syntax.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none

      interface
         subroutine s1(a)
           value :: a
           character*(*) :: a
         end subroutine
      end interface

      end

      subroutine s1(arg)
        value :: arg
        character*(*) :: arg
      end subroutine
