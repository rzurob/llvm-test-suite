!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/05/2005
!*
!*  DESCRIPTION                : allocate (named constants involved in the
!                               source-expr; test the named constants of
!                               intrinsic types)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program falloc05a28
    class(*), allocatable :: x1, x2(:)

    character (20), parameter :: TEST = 'xlftest team members'

    real(8), parameter :: RTEST(10) = (/(i*1.1e0_8, i = 1, 10)/)

    logical(4) precision_r8

    allocate (x1, source=TEST(4:7))

    allocate (x2(5), source=RTEST(::2))

    !! verify results
    select type (x1)
        type is (character(*))
            if (len(x1) /= 4) error stop 1_4

            if (x1 /= TEST(4:7)) error stop 2_4
        class default
            error stop 3_4
    end select

    select type (x2)
        type is (real(8))
            do i = 1, 5
                if (.not. precision_r8(x2(i), RTEST(2*i - 1))) error stop 4_4
            end do
        class default
            error stop 6_4
    end select
end
