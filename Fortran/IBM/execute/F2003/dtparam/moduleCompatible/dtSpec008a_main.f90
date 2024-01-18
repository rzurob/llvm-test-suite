!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/02/3006
!*
!*  DESCRIPTION                : dtparam (section 4.5.8: derived-type-spec)
!                               Case: The function return results put into
!                               select type; use the variable length for the
!                               result.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program dtSpec008a
use m
    class (base(:)), pointer :: b1
    type (base(10)) b2

    logical(4), external :: precision_r4

    b2%data = (/(i*1.0, i=1, 10)/)

    allocate (base(20) :: b1)

    b1%data = (/(i*1.0e1, i=1, 20)/)

    select type (x => b1%add(b2))
        type is (base(*))
            if (x%n /= 20) error stop 1_4

            do i = 1, 10
                if (.not. precision_r4 (x%data(i), i*1.1e1)) error stop 2_4
            end do

            do i = 11, 20
                if (.not. precision_r4 (x%data(i), i*1.0e1)) error stop 3_4
            end do

        class default
            error stop 4_4
    end select

    select type (x => b2%add(b1))
        class is (base(*))
            if (x%n /= 20) error stop 11_4

            do i = 1, 10
                if (.not. precision_r4 (x%data(i), i*1.1e1)) error stop 12_4
            end do

            do i = 11, 20
                if (.not. precision_r4 (x%data(i), i*1.0e1)) error stop 13_4
            end do

        class default
            error stop 14_4
    end select

    select type (x => b2%add(b1, 25))
        class is (base(*))
            if (x%n /= 25) error stop 6_4

            do i = 1, 10
                if (.not. precision_r4 (x%data(i), i*1.0)) error stop 7_4
            end do

            do i = 11, 25
                if (.not. precision_r4 (x%data(i), (i-10)*1.0e1)) error stop 8_4
            end do
        class default
            error stop 10_4
    end select

    select type (x => b1%add(b2, 35))
        type is (base(*))
            if (x%n /= 30) error stop 16_4

            do i = 1, 20
                if (.not. precision_r4 (x%data(i), i*1.0e1)) error stop 17_4
            end do

            do i = 21, 30
                if (.not. precision_r4 (x%data(i), (i-20)*1.0)) error stop 18_4
            end do
        class default
            error stop 19_4
    end select
end
