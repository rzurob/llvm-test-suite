!**********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : cobound01d
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
!*  DESCRIPTION                : Test semantic checking for non-coarray 
!*                               entities being passed to COBOUND intrinsics.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none
      integer, save :: scalar
      integer, save :: arr(10)

      print *, lcobound(scalar)
      print *, lcobound(5)
      print *, lcobound(scalar, 1)
      print *, lcobound(scalar, 1, 4)

      print *, lcobound(arr)
      print *, lcobound([1,2,3])
      print *, lcobound(arr, 1)
      print *, lcobound(arr, 1, 4)

      print *, ucobound(scalar)
      print *, ucobound(6)
      print *, ucobound(scalar, 1)
      print *, ucobound(scalar, 1, 4)

      print *, ucobound(arr)
      print *, ucobound([4.13,5.2,6.0])
      print *, ucobound(arr, 1)
      print *, ucobound(arr, 1, 4)

      end
