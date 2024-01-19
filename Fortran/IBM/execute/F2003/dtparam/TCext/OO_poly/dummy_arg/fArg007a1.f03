! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg007a1.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/06/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (disassociated pointer and
!*                               unallocated allocatables use in intrinsic
!*                               function extends_type_of)
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
    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)   :: id
    end type

    type, extends (base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name
    end type

    class (*), allocatable :: x1 (:)

    class (base(4)), pointer :: b_ptr => null()
end module

program fArg007a1
use m
    class (*), pointer :: x2 (:)
    class (base(4)), allocatable :: b1

    nullify (x2)

    if (.not. extends_type_of (b1, x2)) error stop 1_4

    if (.not. extends_type_of (b_ptr, x1)) error stop 2_4

    if (extends_type_of (x1, b1)) error stop 3_4

    if (extends_type_of (x2, b_ptr)) error stop 4_4
end
