!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: 
! %GROUP: charvalueattrd009.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : charvalueattrd009
!*
!*  PROGRAMMER                 : Bardia Mahjour
!*  DATE                       : Jan. 24, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : providing support for character dummy
!*                               arguments with length other than 1 to  
!*                               have the VALUE attribute (Feature 298120).
!*                                                   
!*  SECONDARY FUNCTIONS TESTED : None 
!*
!*  DRIVER STANZA              : xlf90
!*  REQUIRED COMPILER OPTIONS  : -qdebug=intmsg
!*  REQUIRED RUNTIME OPTIONS   : 
!*
!*  DESCRIPTION                : This diagnostic test, makes sure that
!*                               characters of runtime length with VALUE
!*                               attribute get flagged. This tests the
!*                               VALUE stmt after declaration.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none

      interface
         subroutine s1(a, t)
           integer :: t
           character(len=t) :: a
           value :: a
         end subroutine s1
      end interface

      end

      subroutine s1(arg, i)
        integer :: i
        character(i) :: arg
        value :: arg
      end subroutine s1
