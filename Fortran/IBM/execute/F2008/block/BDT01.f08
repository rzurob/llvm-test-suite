!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2010-11-15
!*
!*  PRIMARY FUNCTIONS TESTED   : F2008 BLOCK
!*  SECONDARY FUNCTIONS TESTED : derived type definition within block
!*  ADAPTED FROM               : -
!*
!*  DESCRIPTION
!*
!*  Derived types can be defined in a block.  We define one and verify that
!*  a variable of that type can be created.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program BDT01
  implicit none
  block
    type :: zort (k)
      integer, kind :: k
      integer(k) :: troz
    end type zort
    type (zort(2)) :: z2, z2a(1)
    z2 = zort(2)(9)
    z2a = zort(2)(10)
    print *, z2%troz, z2a
    if (z2%troz /= 9 .or. z2a(1)%troz /= 10) error stop 2
    z2a = z2
    print *, z2%troz, z2a
    if (z2%troz /= 9 .or. z2a(1)%troz /= 9) error stop 2
  end block
end program BDT01
