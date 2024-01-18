!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : BDT02
!*
!*  PROGRAMMER                 : dforster
!*  DATE                       : 2010-11-15
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : F2008 BLOCK
!*  SECONDARY FUNCTIONS TESTED : derived type redefinition within block
!*  ADAPTED FROM               : -
!*
!*  DESCRIPTION
!*
!*  A derived type can be redefined in a block, so that the containing definition
!*  is not available for use in the block.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program BDT02
  implicit none
  type :: zort (l)
    integer, len :: l
    integer :: troz(l)
  end type zort
  type (zort(2)) :: z2
  z2 = zort(2)(9)
  print *, z2%troz
  block
    type :: zort (k)
      integer, kind :: k
      integer(k) :: troz
    end type zort
    type (zort(2)) :: z2
    z2 = zort(2)(9)
    print *, z2%troz
  end block
  print *, z2%troz
end program BDT02
