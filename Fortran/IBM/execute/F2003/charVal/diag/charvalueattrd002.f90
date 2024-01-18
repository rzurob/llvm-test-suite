!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: 
! %GROUP: charvalueattrd002.f
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
!*  TEST CASE TITLE            : charvalueattrd002
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
!*                               characters of assumed length with VALUE
!*                               attribute get flagged. This tests the
!*                               VALUE attribute as part of the declaration.
!*                               Testing "character(*)" syntax.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none

      interface
         subroutine s1(a)
           character(*), value :: a
         end subroutine
      end interface
      
      end

      subroutine s1(arg)
        character(*), value :: arg
      end subroutine 
