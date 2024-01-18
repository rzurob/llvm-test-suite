!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : BDT02d
!*
!*  PROGRAMMER                 : dforster
!*  DATE                       : 2010-11-15
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : F2008 BLOCK
!*  SECONDARY FUNCTIONS TESTED : derived type redefinition and structure constructor within block
!*  ADAPTED FROM               : -
!*
!*  DESCRIPTION
!*
!*  A derived type can be redefined in a block, so that the containing definition
!*  is not available for use in the block, unless it is made available through an
!*  IMPLICIT statement.  In that case, however, the structure constructor is not
!*  available.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program BDT02d
  type :: zort
    integer :: troz
  end type zort
  implicit type(zort) (z)
  z = zort(9)
  print *, z2%troz
  block
    type :: zort
      real :: troz
    end type zort
    z2 = zort(9) ! accesses SC in containing scope
    print *, z2%troz
  end block
  print *, z2%troz
end program BDT02d
