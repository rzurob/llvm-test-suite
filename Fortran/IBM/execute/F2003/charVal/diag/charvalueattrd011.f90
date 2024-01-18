!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: 
! %GROUP: charvalueattrd011.f
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
!*  TEST CASE TITLE            : charvalueattrd011
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
!*                               characters of deferred length with VALUE
!*                               attribute get flagged. This tests the
!*                               VALUE attribute in the declaration.
!*                               This tests allocatable deferred length.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none

      interface
         subroutine s1(a)
           character(:), allocatable, value :: a
         end subroutine s1
      end interface

      end

      subroutine s1(arg)
           character(:), allocatable, value :: arg        
      end subroutine s1
