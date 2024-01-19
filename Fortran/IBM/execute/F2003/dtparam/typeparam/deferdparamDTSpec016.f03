! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/13/2006
!*
!*  DESCRIPTION                : dtparam (section 4.2: type parameters)
!                               Case: Deferred type-parameters:
!                               defined/undefined during argument association;
!                               use allocatable array entities.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(n)
        integer, len :: n

        real(4) :: data4(n)
        real(8) :: data8(n/2)
    end type

    contains

    subroutine resetBaseAlloc (b)
        class(base(:)), allocatable, intent(out) :: b(:)
    end subroutine

    subroutine printBaseAlloc (b)
        class(base(:)), allocatable, intent(in) :: b(:)

        if (.not. allocated(b)) then
            print *, 'input data not allocated'
        else
            do i = lbound(b,1), ubound(b,1)
                write(*, '(7f10.2)') b(i)%data4
                write(*, '(6g15.5)') b(i)%data8
            end do
        end if
    end subroutine

    subroutine reallocBase (b, r1)
        class(base(:)), allocatable, intent(inout) :: b(:)
        real, intent(in) :: r1(:,:)

        call resetBaseAlloc(b)

        allocate (base(size(r1, 1)):: b(size(r1, 2)))

        do i = 1, size(r1, 2)
            b(i)%data4 = r1(:, i)
            b(i)%data8 = r1(:size(r1,1) - 1:2, i)
        end do
    end subroutine
end module

program deferdparamDTSpec016
use m
    class(base(:)), allocatable :: b1(:)

    real, allocatable :: r1(:,:)

    call printBaseAlloc (b1)

    allocate (base(10) :: b1(3))

    do i = 1, 3
        b1(i)%data4 = (/(1.0e2+j, j=1,10)/)
        b1(i)%data8 = (/(1.0e1+j, j=1,5)/)
    end do

    call printBaseAlloc (b1)

    allocate (r1(7, 2))

    r1 = reshape((/((i*10+j, i=1, 7), j=1,2)/), (/7,2/))

    call reallocBase (b1, r1)

    call printBaseAlloc (b1)

    call resetBaseAlloc (b1)
    call resetBaseAlloc (b1)

    deallocate (r1)

    allocate (r1(20, 3))

    r1 = reshape((/((i*10+j, i=1, 20), j=1,3)/), (/20, 3/))

    call reallocBase (b1, r1)

    call printBaseAlloc (b1)
end
