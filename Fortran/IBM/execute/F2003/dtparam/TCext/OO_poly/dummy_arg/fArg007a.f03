! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg007a.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/06/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (nullified pointer and
!*                               unallocated allocatables allowed in the
!*                               intrinsic function same_type_as)
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
    type base(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
    end type

    type, extends(base) :: child    ! (4,20)
        integer(k1) :: id
    end type

    class (base(4,:)), pointer :: b_ptr => null()

    class (base(4,:)), allocatable :: b_alloc(:)
end module

program fArg007a
use m
    class (*), pointer :: x1 (:)
    class (*), allocatable :: x2

    nullify (x1)

    if (.not. same_type_as (b_ptr, b_alloc)) error stop 1_4

    if (same_type_as (x1, x2)) error stop 2_4
end