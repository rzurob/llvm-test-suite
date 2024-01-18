! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/class/fclass021.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/04/2005
!*
!*  DESCRIPTION                : CLASS keyword (test that variables in the
!                               where-assignment-statement can be affected by
!                               the evaluation of the mask-expression)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        class(*), allocatable :: data(:)
    end type

    contains

    function largeArray (b1, n)
        class (base(4,*)), intent(inout) :: b1(:)
        integer, intent(in) :: n

        logical(4) largeArray (size(b1))

        do i = 1, size(b1)
            if (allocated (b1(i)%data) .and. (size(b1(i)%data) >= n)) then
                largeArray (i) = .true.
            else
                if (allocated (b1(i)%data)) deallocate (b1(i)%data)

                allocate (b1(i)%data(1), source=1_8)

                largeArray (i) = .false.
            end if
        end do
    end function
end module

program fclass021
use m
    type (base(4,20)) :: b1(100)
    logical(4) precision_r4, mask(100)

    b1(::2) = (/(base(4,20)((/(i, i=1, j)/)), j = 1, 100, 2)/)

    mask = largeArray (b1, 20)

!    where (largeArray (b1, 20))
    do i = 1, 100
        if(mask(i)) b1(i) = base(4,20) ((/1.0_4, 2.0_4/))
    end do
!    endwhere

    !! verify results
    do i = 1, 100
        if (.not. allocated (b1(i)%data)) error stop 100_4
    end do


    do i = 21, 100, 2
        if (size(b1(i)%data) /= 2) error stop 1_4

        select type (d => b1(i)%data)
            type is (real(4))
                if(.not. precision_r4 (d(1), 1.0)) error stop 2_4
                if(.not. precision_r4 (d(2), 2.0)) error stop 3_4
            class default
                error stop 4_4
        end select
    end do

    do i = 1, 20
        if (size(b1(i)%data) /= 1) error stop 5_4

        select type (i8 => b1(i)%data)
            type is (integer(8))
                if (i8(1) /= 1) error stop 6_4
            class default
                error stop 7_4
        end select
    end do

    do i = 22, 100, 2
        if (size(b1(i)%data) /= 1) error stop 8_4

        select type (i8 => b1(i)%data)
            type is (integer(8))
                if (i8(1) /= 1) error stop 9_4
            class default
                error stop 10_4
        end select
    end do
end
