! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp /tstdev/F2003/allocEnh/argAssociation/dummyArg007a.f
! opt variations: -qnok -qnol -qnodeferredlp

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/18/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Test that the derived type dummy allocatable
!                               argument used in the intrinsic assignment:
!                               scalar case of dummyArg007.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(n1,k1,k2)    ! (20,8,4)
        integer, kind         :: k1,k2
        integer, len          :: n1
        real(k1), allocatable :: data(:)
        logical(k2), pointer  :: mask(:)

        contains

        procedure :: setMask => setBaseMask
        procedure :: reduce => reduceBaseData
    end type

    contains

    subroutine copyBase (b1, b2)
        type(base(:,8,4)), allocatable :: b1, b2

        b1 = b2
    end subroutine

    subroutine setBaseMask (b, d1, cmp)
        class(base(*,8,4)) b
        double precision d1
        procedure(logical) cmp

        if (allocated(b%data)) then
            allocate(b%mask(size(b%data)))

            b%mask = [(cmp(b%data(i), d1),&
                i=lbound(b%data,1), ubound(b%data,1))]
        else
            nullify(b%mask)
        end if
    end subroutine

    subroutine reduceBaseData (b)
        class(base(*,8,4)) b

        if (associated(b%mask)) then
            b%data = pack(b%data, b%mask)

            nullify(b%mask)
        else
            stop 10
        end if
    end subroutine
end module

module m1
use m, only: base
    type dataType(k3,n2)    ! (4,20)
        integer, kind :: k3
        integer, len  :: n2
        double precision, allocatable :: data(:)
    end type
    contains

    subroutine moveData (d1, b1)
        type(base(:,8,4)), allocatable :: b1
        class(dataType(4,*)) d1

        if (.not. allocated(b1)) allocate(base(20,8,4) :: b1)

        call move_alloc (d1%data, b1%data)
    end subroutine

    subroutine setMask (b1, d1, func)
        type(base(:,8,4)), allocatable :: b1
        real(8) d1
        procedure(logical) func

        call b1%setMask(d1, func)
    end subroutine
end module


program dummyArg007
use m1
use m, only: copyBase
    type(base(:,8,4)), allocatable :: b1, b2

    type(dataType(4,:)), allocatable :: d1

    logical, external :: less, precision_r8

    d1 = dataType(4,20)([(i, i=1,100)])

    call moveData (d1, b1)

    call setMask (b1, 20.2d0, less)

    call copyBase (b2, b1)

    call b1%reduce


    !! verify d1, b1 and b2
    if (allocated(d1%data)) error stop 2_4

    if (associated(b1%mask) .or. (.not. associated(b2%mask))) error stop 3_4

    if ((size(b1%data) /= 20) .or. (size(b2%data) /= 100) .or. &
        (size(b1%mask) /= 100)) error stop 4_4

    do j = 1, 20
        if (.not. precision_r8 (b1%data(j), j*1.0d0)) error stop 5_4

        if (.not. precision_r8 (b2%data(j), j*1.0d0)) error stop 6_4

        if (.not. b2%mask(j)) error stop 7_4
    end do

    do i = 21, 100
        if (b2%mask(i)) error stop 8_4

        if (.not. precision_r8 (b2%data(i), i**1.0d0)) error stop 9_4
    end do

    deallocate (b2%mask)
end

logical function less (d1, d2)
    double precision d1, d2
    less = d1 < d2
end function