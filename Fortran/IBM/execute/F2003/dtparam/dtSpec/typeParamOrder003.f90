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
!*  DATE                       : 02/06/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.5.8: derived-type-spec)
!                               Case: For parameterized dummy-arg with deferred
!                               type parameter.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k1, l1, k2, l2)
        integer, len :: l1, l2 = 10
        integer, kind :: k1 = 4, k2

        real(k1) :: data(l1)
        complex(k2) :: cx(l2)
    end type

    type, extends(base) :: child (l3, k3)
        integer, kind :: k3
        integer, len :: l3 = 20

        integer(k3) :: id(l3)
    end type
end module

module allocateStruct
use m
    contains

    subroutine allocateBase4_8 (b, r1, cx1)
        class(base(4,:,8,:)), allocatable, intent(out) :: b
        real(4), intent(in) :: r1(:)
        complex(8), intent(in) :: cx1(:)

        allocate (base(4, size(r1), 8, size(cx1)):: b)

        b%data = r1
        b%cx = cx1
    end subroutine

    subroutine allocateChild8_4_8 (c, d1, cx1, i1)
        class (child(8,:,4,:,:,8)), pointer :: c
        real(8), intent(in) :: d1(:)
        complex(4), intent(in) :: cx1(:)
        integer(8), intent(in) :: i1(:)

        allocate (child(8,size(d1), 4,size(cx1), size(i1), 8) :: c)

        c%data = d1
        c%cx = cx1
        c%id = i1
    end subroutine
end module

program typeParamOrder003
use allocateStruct
    class (base(4,:,8,:)), allocatable :: b1
    class (child(8,:,4,:,:,8)), pointer :: c1 => null()

    real(4) :: r1 (20)
    double precision, allocatable :: d1(:)

    complex(4), pointer :: cx1(:)
    complex(8) :: dcx1(50)

    integer(8) :: i1(100)

    logical(4), external :: precision_r4, precision_r8, precision_x8, &
            precision_x6

    r1 = (/(i*1.0, i=1,20)/)

    allocate (d1(35), source=log((/(i*1.0d0, i=1,35)/)))

    allocate (cx1(22), source=cmplx((/r1, 21.0, 22.0/), &
            (/22.0, 21.0, r1(20:1:-1)/)))

    dcx1 = cmplx(log((/(i*1.0d0, i=1,50)/)), log((/(i*1.2d1, i=1,50)/)), 8)

    i1 = (/(j, j=100, 1, -1)/)

    call allocateBase4_8 (b1, r1, dcx1)
    call allocateChild8_4_8 (c1, d1, cx1, i1)

    !!verify
    if ((.not. allocated(b1)) .or. (.not. associated(c1))) error stop 1_4

    if ((size(b1%data) /= 20) .or. (size(b1%cx) /= 50)) error stop 2_4

    if ((size(c1%data) /= 35) .or. (size(c1%cx) /= 22) .or. &
        (size(c1%id) /= 100)) error stop 3_4

    do i = 1, 20
        if (.not. precision_r4(b1%data(i), i*1.0)) error stop 5_4
    end do

    do i = 1, 50
        if (.not. precision_x6(b1%cx(i), cmplx(log(i*1.0d0), log(i*1.2d1), 8))) &
                error stop 6_4
    end do

    do i = 1, 35
        if (.not. precision_r8(c1%data(i), log(i*1.0d0))) error stop 7_4
    end do

    do i = 1, 22
        if (.not. precision_x8(c1%cx(i), (i*1.0, (23-i)*1.0))) error stop 8_4
    end do

    if (any(c1%id /= (/(j, j=100, 1, -1)/))) error stop 9_4
end
