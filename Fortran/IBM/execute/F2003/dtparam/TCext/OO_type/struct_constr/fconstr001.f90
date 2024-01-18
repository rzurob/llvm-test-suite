! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr001.f
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
    type base(k1,k2)    ! (4,4)
        integer, kind     :: k1,k2
        integer(k1)       :: id = 1
        real(k2), pointer :: value => null()
    end type

    type, extends(base) :: child(k3,n1,k4)    ! (4,4,1,20,8)
        integer, kind             :: k3,k4
        integer, len              :: n1
        character(kind=k3,len=n1) :: name = 'child type'
        real(k4), allocatable     :: extendedValue (:)
    end type

    type (base(4,4)), save :: b1_m
    type (child(4,4,1,20,8)), save :: c1_m
end module

program fconstr001
use m

    type, extends (base) :: secondChild(k5)    ! (4,4,4)
        integer, kind     :: k5
        real(k5), pointer :: extendedValue (:) => null()
    end type

    type (base(4,4)) :: b1
    type (child(4,4,1,20,8)) :: c1
    type (secondChild(4,4,4)) :: s1

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
