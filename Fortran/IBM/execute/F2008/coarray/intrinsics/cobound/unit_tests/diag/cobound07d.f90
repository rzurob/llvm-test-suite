!**********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : cobound07d
!*
!*  PROGRAMMER                 : Bardia Mahjour
!*  DATE                       : Aug 20, 2010
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Diagnostic
!*                                                   
!*  SECONDARY FUNCTIONS TESTED : None 
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  DESCRIPTION                : Diagnostic: actual argument associated 
!*                               with DIM must not be an optional dummy arg
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      implicit none
      contains

      subroutine foo(arg)
       integer, optional :: arg
       integer, save :: coarr[*]

       print *, lcobound(coarr, dim=arg, kind=4)
       print *, lcobound(coarr, arg, 4)
       print *, lcobound(coarr, arg)

       print *, ucobound(coarr, dim=arg, kind=4)
       print *, ucobound(coarr, arg, 4)
       print *, ucobound(coarr, arg)

      end subroutine

      end
