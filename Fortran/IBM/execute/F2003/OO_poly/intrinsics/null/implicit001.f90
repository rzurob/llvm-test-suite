!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/null
! PROGRAMMER                 : Yong Du
! DATE                       : 03/02/2005
! PRIMARY FUNCTIONS TESTED   : null
! DRIVER STANZA              : xlf90
! DESCRIPTION                : The type of MOLD is implicitly declared.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY : 04/05/05
!                       Init : yongdu@ca.ibm.com
!                   Comments : 1) Updated the DESCRIPTION part.
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type Base
        integer i
    end type

    type, extends(Base) :: Child
        integer j
    end type
end module

program implicit001
use m
    implicit type(Child) (c)
    implicit class(Base) (b)
    implicit class(*) (u)

    pointer :: c1
    allocatable :: b1(:,:)
    pointer :: u1

    if(associated(c1)) error stop 1_4
    if(allocated(b1)) error stop 2_4
    if(associated(u1)) error stop 3_4

    if(associated(null(c1))) error stop 4_4
    if(allocated(null(b1))) error stop 5_4
    if(associated(null(u1))) error stop 6_4

    if(same_type_as(c1, b1)) error stop 7_4
    if(same_type_as(b1, u1)) error stop 8_4
    if(.NOT. extends_type_of(c1, b1)) error stop 9_4
    if(.NOT. extends_type_of(b1, u1)) error stop 10_4

    if(same_type_as(null(c1), null(b1))) error stop 11_4
    if(same_type_as(null(b1), null(u1))) error stop 12_4
    if(.NOT. extends_type_of(null(c1), null(b1))) error stop 13_4
    if(.NOT. extends_type_of(null(b1), null(u1))) error stop 14_4
    if(extends_type_of(null(u1), null(c1))) error stop 15_4
end
