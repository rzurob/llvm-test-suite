! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 12, 2003
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (default initialization)
!*                               pointer types are disassociated; allocatable
!*                               not allocated
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012
module m
    type base
        integer*4 :: id = 1
        real*4, pointer :: value => null()
    end type

    type, extends(base) :: child
        character(20) :: name = 'child type'
        real*8, allocatable :: extendedValue (:)
    end type

    type (base), save :: b1_m
    type (child), save :: c1_m
end module

program fconstr001
use m

    type, extends (base) :: secondChild
        real*4, pointer :: extendedValue (:) => null()
    end type

    type (base) :: b1
    type (child) :: c1
    type (secondChild) :: s1

    if (associated (b1_m%value)) error stop 1_4
    if (b1_m%id /= 1) error stop 2_4

    if (associated (b1%value)) error stop 3_4
    if (b1%id /= 1) error stop 4_4

    if (associated (c1_m%value)) error stop 5_4
    if (c1_m%id /= 1) error stop 6_4
    if (c1_m%name /= 'child type') error stop 7_4
    if (allocated (c1_m%extendedValue)) error stop 8_4

    if (associated (c1%value)) error stop 9_4
    if (c1%id /= 1) error stop 10_4
    if (c1%name /= 'child type') error stop 11_4
    if (allocated (c1%extendedValue)) error stop 12_4

    if (associated (s1%value)) error stop 13_4
    if (associated (s1%extendedValue)) error stop 14_4
    if (s1%id /= 1) error stop 15_4
end
