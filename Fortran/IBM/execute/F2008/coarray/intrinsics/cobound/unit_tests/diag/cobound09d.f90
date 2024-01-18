!**********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : cobound09d
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
!*  DESCRIPTION                : Diagnostic: shape of the result
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      implicit none

      integer, save :: coarr(10)[10,10,*]
      integer, save :: cos[10,10,*]
      integer :: res1(3)
      integer :: res2(4)
      integer :: res3(2)

      ! these should be fine:
      res1 = lcobound(coarr)
      res1 = lcobound(cos)
      res1 = ucobound(coarr)
      res1 = ucobound(cos)

      ! these should issue messages
      res2 = lcobound(coarr)
      res2 = lcobound(cos)
      res2 = ucobound(coarr)
      res2 = ucobound(cos)

      res3 = lcobound(coarr)
      res3 = lcobound(cos)
      res3 = ucobound(coarr)
      res3 = ucobound(cos)

      end
