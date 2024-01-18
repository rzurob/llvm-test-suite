!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/3/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               miscellaneous (actual-arg being a function
!                               result that does uses allocatable dummy-arg in
!                               the intrinsic assignment)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

    real(8), allocatable :: r1(:)

    real(8) r2(10000)

    logical(4), external :: precision_r8

    r2 = [(i*2.0001d-4, i=1,10000)]
    r1 = r2

    call assgnR1R2 (r1, genR1(r1, r2))

    if (.not. allocated(r1)) error stop 1_4

    if (size(r1) /= 10000) error stop 2_4

    !! verify r1
    do i = 1, 4999
        if (.not. precision_r8 (r1(i), dsqrt(i*2.0001d-4))) error stop 3_4
    end do

    do i = 5000, 10000
        if (.not. precision_r8 (r1(i), i*2.0001d-4)) error stop 4_4
    end do

    contains

    real(8) function genR1 (r1, r2)
        real(8), allocatable, intent(out) :: r1(:)
        real(8), intent(in) :: r2(:)

        dimension genR1(size(r2))

        r1 = sqrt(r2)

        genR1 = max(r1, r2)
    end function

    subroutine assgnR1R2 (r1, r2)
        real(8), allocatable :: r1(:)
        real(8) :: r2(:)

        r1 = r2
    end subroutine
    end
