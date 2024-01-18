!***********************************************************************
!* =====================================================================
!* XL Fortran Test Case                            IBM INTERNAL USE ONLY
!* =====================================================================
!* TEST BUCKET                : OO_poly/intrinsics/typeQuery
!* PROGRAMMER                 : Yong Du
!* DATE                       : 11/03/2004
!* PRIMARY FUNCTIONS TESTED   : extends_type_of(A, MOLD)
!* SECONDARY FUNCTIONS TESTED : same_type_as(A, B)
!* DRIVER STANZA              : xlf90
!* DESCRIPTION                : MOLD or A is an array constructor, or
!*                              is initialized using array constructor.
!* =====================================================================
!* REVISION HISTORY
!*                   MM/DD/YY : 06/28/05
!*                       Init : yongdu@ca.ibm.com
!*                   Comments : 1) fix bug on line 68 of original file
!*                              2) fix bug on zzrc calls 16 and 17 of
!*                                 original file
!*                              3) remove TRUN header
!* =====================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type Base
        integer i
    end type

    type, extends(Base) :: Child
        character(10) :: c
    end type
end module

program array006
use m
    class(*), pointer :: arg1(:,:) => null()
    class(*), allocatable :: arg2(:,:)
    class(*), pointer :: i1(:)
    type(Base), target :: arg3(2,2) = &
     reshape((/Base(1),Base(2),Base(3),Base(4)/), (/2,2/))
    integer, target :: arg4(2,2) = reshape((/1,2,3,4/), (/2,2/))

    if(.NOT. extends_type_of((/Child(1,"a"),Child(2,"b")/), arg1)) &
     error stop 1_4
    if(extends_type_of(arg2, (/Base(1),Base(2)/))) error stop 2_4

    if(same_type_as((/Child(1,"a"),Child(2,"b")/), arg1)) error stop 3_4
    if(same_type_as(arg2, (/Base(1),Base(2)/))) error stop 4_4

    arg1 => arg4
    if(.NOT. extends_type_of(arg1, arg2)) error stop 5_4
    if(extends_type_of(arg2, arg1)) error stop 6_4

    if(same_type_as(arg1, arg2)) error stop 7_4
    if(same_type_as(arg2, arg1)) error stop 8_4

    allocate(i1(3), SOURCE=(/1,2,3/))
    if(.NOT. same_type_as(arg1, i1)) error stop 9_4

    arg1 => arg3
    if(.NOT. extends_type_of((/Child(1,"a"),Child(2,"b")/), arg1)) &
     error stop 10_4
    if(extends_type_of(arg1, (/Child(1,"a"),Child(2,"b")/))) error stop 11_4

    if(same_type_as((/Child(1,"a"),Child(2,"b")/), arg1)) &
     error stop 12_4
    if(same_type_as(arg1, (/Child(1,"a"),Child(2,"b")/))) error stop 13_4

    if(.NOT. extends_type_of((/Child(1,"a"),Child(2,"b")/), &
     (/Base(1),Base(2),Base(3)/))) error stop 14_4
    if(extends_type_of((/Base(1),Base(2),Base(3)/), &
     (/Child(1,"a"),Child(2,"b")/))) error stop 15_4

    if(.NOT. extends_type_of((/Child(1,"a"),Child(2,"b")/), &
     (/Base(1),Base(2),Base(3)/))) error stop 16_4
    if(extends_type_of((/Base(1),Base(2),Base(3)/), &
     (/Child(1,"a"),Child(2,"b")/))) error stop 17_4
end
