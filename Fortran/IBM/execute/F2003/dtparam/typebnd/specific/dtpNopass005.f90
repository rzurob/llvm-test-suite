!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 05/01/2007
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : derived type parameter
!                               specific type bound (Use of nopass binding of a
!                               derived type to manage the resource allocations
!                               in a module)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, n)
        integer, kind :: k
        integer, len :: n

        real(k) :: data(n)

        contains

        procedure, nopass :: updateAgent4
        procedure, nopass :: updateAgent8

        procedure, nopass :: restore4
        procedure, nopass :: restore8
    end type

    real(4), allocatable, save, protected :: backup4(:)
    real(8), allocatable, save, protected :: backup8(:)

    type(base(4,:)), allocatable, save, protected :: agent4
    type(base(8,:)), allocatable, save, protected :: agent8

    contains

    subroutine updateAgent4 (r1)
        real(4), intent(in) :: r1(:)

        if (allocated(agent4)) backup4 = agent4%data

        agent4 = base(4, size(r1))(r1)
    end subroutine

    subroutine updateAgent8 (d1)
        real(8), intent(in) :: d1(:)

        if (allocated(agent8)) backup8 = agent8%data

        agent8 = base(8, size(d1))(d1)
    end subroutine

    subroutine restore4 ()
        if (allocated(backup4)) then
            agent4 = base(4, size(backup4))(backup4)

            deallocate (backup4)
        end if
    end subroutine

    subroutine restore8
        if (allocated(backup8)) then
            agent8 = base(8, size(backup8))(backup8)

            deallocate (backup8)
        end if
    end subroutine
end module

program dtpNopass005
use m
    real(4), allocatable :: r1(:), r2(:)
    real(8), pointer :: d1(:), d2(:)

    class(base(16,:)), pointer :: b1

    logical(4), external :: precision_r4, precision_r8

    r1 = [(i, i = 1, 200)]

    r2 = log(r1(10:))

    allocate (d1(150), d2(0:199))

    d1 = sin([(i*4.5d-3, i=1,150)])

    d2 = asin([d1, d1(1:50)])

    allocate (base(16,20) :: b1)

    call b1%updateAgent4 (r1)

    call b1%updateAgent8 (d1)

    !! verify backup4, backup8, agent4 and agent8
    if ((.not. allocated(agent4)) .or. (.not. allocated(agent8))) error stop 1_4

    if ((size(agent4%data) /= 200) .or. (size(agent8%data) /= 150)) error stop 2_4

    do i = 1, 200
        if (.not. precision_r4(agent4%data(i), i*1.0_4)) error stop 3_4
    end do

    do i = 1, 150
        if (.not. precision_r8(agent8%data(i), sin(i*4.5d-3))) error stop 4_4
    end do

    if (allocated(backup4) .or. allocated(backup8)) error stop 5_4

    !! update
    call b1%updateAgent4 (r2)

    call b1%updateAgent8 (d2)

    !! verify backup4, backup8, agent4 and agent8
    if ((.not. allocated(backup4)) .or. (.not. allocated(backup8))) error stop 6_4

    if ((size(agent4%data) /= 191) .or. (size(agent8%data) /= 200)) error stop 7_4
    if ((size(backup4) /= 200) .or. (size(backup8) /= 150)) error stop 8_4

    do i = 1, 191
        if (.not. precision_r4(agent4%data(i), log(i+9.0_4))) error stop 9_4
    end do

    do i = 1, 150
        if (.not. precision_r8(backup8(i), sin(i*4.5d-3))) error stop 10_4

        if (.not. precision_r4(backup4(i), i*1.0_4)) error stop 11_4

        if (.not. precision_r8(agent8%data(i), i*4.5d-3)) error stop 12_4
    end do

    do i = 151, 200
        if (.not. precision_r4(backup4(i), i*1.0_4)) error stop 14_4

        if (.not. precision_r8(agent8%data(i), (i-150)*4.5d-3)) error stop 13_4
    end do

    !! last, restore the original values
    call b1%restore4
    call b1%restore8

    if (allocated(backup4) .or. allocated(backup8)) error stop 18_4

    if ((size(agent4%data) /= 200) .or. (size(agent8%data) /= 150)) error stop 15_4

    do i = 1, 200
        if (.not. precision_r4(agent4%data(i), i*1.0_4)) error stop 16_4
    end do

    do i = 1, 150
        if (.not. precision_r8(agent8%data(i), sin(i*4.5d-3))) error stop 17_4
    end do

end
