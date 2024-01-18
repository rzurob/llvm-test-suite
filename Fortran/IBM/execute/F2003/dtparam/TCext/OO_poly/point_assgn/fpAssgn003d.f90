! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp /tstdev/OO_poly/point_assgn/fpAssgn003d.f
! opt variations: -qnok -qnol -qnodeferredlp

! SCCS ID Information
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/06/2005
!*
!*  DESCRIPTION                : data pointer assignment (for the associated(),
!                               the TARGET used in this intrinsic has to be of a
!                               data that is allowed as if in a data pointer
!                               assignment)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
    end type

    type, extends(base) :: child(k2,n2)    ! (4,20,4,20)
        integer, kind :: k2
        integer, len  :: n2
    end type
end module

program fpAssgn003d
use m

    class (base(4,:)), pointer :: b_ptr
    class (child(4,:,4,:)), pointer :: c_ptr

    type(child(4,20,4,20)), target :: c = child(4,20,4,20)()

    integer*4, target :: i1

    class (*), pointer :: x

    b_ptr => c
    c_ptr => c

    x => c

    print *, associated(b_ptr, x)   !<-- this is illegal
    print *, associated (b_ptr, i1) !<-- also illegal

    print *, associated (c_ptr, b_ptr)  !<-- also illegal
    print *, associated (c_ptr, c_ptr%base)  !<-- again illegal
end
