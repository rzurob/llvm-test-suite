!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 24, 2006
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
!*                               characters of deferred length with VALUE
!*                               attribute get flagged. This tests the
!*                               VALUE stmt after the declaration.
!*                               This tests pointer deferred length.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none

      interface
         subroutine s1(a)
           character(:), pointer :: a
           value :: a
         end subroutine s1
      end interface

      end

      subroutine s1(arg)
           character(:), pointer :: arg
           value :: arg
      end subroutine s1
