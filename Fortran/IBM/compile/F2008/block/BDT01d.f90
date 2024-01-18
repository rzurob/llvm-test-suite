!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : BDT01d
!*
!*  PROGRAMMER                 : dforster
!*  DATE                       : 2010-11-15
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : F2008 BLOCK
!*  SECONDARY FUNCTIONS TESTED : derived type definition within block used outside
!*  ADAPTED FROM               : BDT01
!*
!*  DESCRIPTION
!*
!*  Derived types can be defined in a block.  That type cannot be accessed outside
!*  the block.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program BDT01d
  implicit none
  block
    type :: zort (k)
      integer, kind :: k
      integer(k) :: troz
    end type zort
  end block
  type (zort(2)) :: z2
  z2 = zort(2)(9)
end program BDT01d
