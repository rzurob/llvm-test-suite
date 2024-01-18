! GB DTP extension using:
! ftcx_dtp -qck -qk -qnol -qreuse=base /tstdev/F2003/allocEnh/argAssociation/dummyArg015.f
! opt variations: -qnock -qnok -ql -qreuse=none

! SCCS ID Information
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/1/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Polymorphic allocatable dummy-arg array used as
!                               expr in intrinsic assignment.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1)    ! (4)
        integer, kind :: k1
        character(:), allocatable :: name
    end type

    type, extends(base) :: child    ! (4)
        real(k1), allocatable :: data(:)
    end type


    contains

    subroutine assgn (c1, b1)
        type(child(4)), allocatable :: c1(:)
        class(base(4)), allocatable :: b1(:)

        select type (b1)
            class is (child(4))
                c1 = b1

            class default
                if (allocated(c1)) deallocate (c1)

                allocate(c1(lbound(b1,1):ubound(b1,1)))

                c1%base = b1
        end select
    end subroutine
end module

program dummyArg015
use m
    character(*), parameter :: alphabet(26) = [(achar(64+i), i=1,26)]

    type(child(4)), allocatable :: c1(:)

    class(base(4)), allocatable :: b1(:), b2(:)

    logical(4), external :: precision_r4

    allocate (b1(0:9), source=[(child(4)('xlftest '// repeat(alphabet(i), i),&
        [(j, j=1,i)]), i=1,10)])

    call assgn (c1, b1)

    if (.not. allocated(c1)) error stop 1_4

    if ((lbound(c1,1) /= 0) .or. (ubound(c1,1) /= 9)) error stop 2_4

    do i = 0, 9
        if (c1(i)%name%len /= (9+i)) error stop 3_4

        if (c1(i)%name /= 'xlftest ' // repeat(achar(65+i), i+1)) error stop 4_4

        if (.not. allocated(c1(i)%data)) error stop 5_4

        if (size(c1(i)%data) /= i+1) error stop 6_4

        do j = 1, i+1
            if (.not. precision_r4 (c1(i)%data(j), j*1.0_4)) error stop 7_4
        end do
    end do

    !! 2nd test
    call move_alloc(b1, b2)

    allocate (b1(2:11), source=[(base(4)(alphabet(j)), j=26,17,-1)])

    deallocate (b2)

    call assgn (c1, b1)

    if (.not. allocated (c1)) error stop 10_4

    if ((lbound(c1,1) /= 2) .or. (ubound(c1,1) /= 11)) error stop 11_4

    do i = 2, 11
        if (c1(i)%name /= achar(92-i)) error stop 12_4

        if (allocated(c1(i)%data)) error stop 13_4
    end do
end
