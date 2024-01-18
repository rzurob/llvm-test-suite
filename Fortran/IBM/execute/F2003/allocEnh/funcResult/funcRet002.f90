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
!*  DATE                       : 09/27/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Test a function return array used as an expr in
!                               intrinsic assignment for an allocatable
!                               variable; type are intrinsic types (character
!                               and complex) and verify the bounds are set
!                               correctly.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program funcRet002
    complex, allocatable :: cx1(:), cx2(:), cx3(:)
    character(:), allocatable :: c1(:), c2(:), c3(:), c4(:)

    logical(4), external :: precision_x8

    allocate (cx1(0:99), cx2(0:99))

    allocate (character(20) :: c1(0:9), c2(0:99), c3(0:99))

    !! size match between cx1 and funcReturn, no reallocation
    cx1 = genComplxArray ((0.0, 0.0), 100, 20)

    !! size mis-match between cx1 and funcReturn, reallocation occurrs
    cx2 = genComplxArray ((0.0, 0.0), 150, 30)

    !! unallocated cx3 will be automatically allocated
    cx3 = genComplxArray ((1.0, 1.0), 50, 10)


    !! length and size both match between c1 and funcReturn; no reallocation
    c1 = genCharArray (20, 10, 10)

    !! length mis-match between c2 and funcReturn; reallocation happens
    c2 = genCharArray (10, 100, -10)

    !! size mis-match between c3 and funcReturn; reallocation happens
    c3 = genCharArray (20, 50, -100)

    !! c4 is unallocated; will be automatically allocated
    c4 = genCharArray (100, 100, 100)



    !! verify cx1, cx2 and cx3
    if ((lbound(cx1,1) /= 0) .or. (ubound(cx1,1) /= 99)) error stop 1_4

    if ((lbound(cx2,1) /= 1) .or. (ubound(cx2,1) /= 150)) error stop 2_4

    if ((lbound(cx3,1) /= 1) .or. (ubound(cx3,1) /= 50)) error stop 3_4

    do i = 0, 99
        if (.not. precision_x8(cx1(i), cmplx(20+i, 20+i,4))) error stop 4_4
    end do

    do i = 1, 150
        if (.not. precision_x8(cx2(i), cmplx(29+i,29+i,4))) error stop 5_4
    end do

    do i = 1, 50
        if (.not. precision_x8(cx3(i), cmplx(10+i,10+i,4))) error stop 6_4
    end do


    !!! verify c1, c2, c3 and c4
    if ((lbound(c1,1) /= 0) .or. (ubound(c1,1) /= 9)) error stop 7_4
    if (c1%len /= 20) error stop 8_4

    if ((lbound(c2,1) /= 1) .or. (ubound(c2,1) /= 100)) error stop 9_4
    if (c2%len /= 10) error stop 10_4

    if ((lbound(c3,1) /= 1) .or. (ubound(c3,1) /= 50)) error stop 11_4
    if (c3%len /= 20) error stop 12_4

    if ((lbound(c4,1) /= 1) .or. (ubound(c4,1) /= 100)) error stop 13_4
    if (c4%len /= 100) error stop 14_4

    do i = 0, 9
        if (c1(i) /= 't'//achar(10+i)) error stop 15_4
    end do

    do i = 1, 100
        if (c2(i) /= 't'//achar(abs(i-11))) error stop 16_4
        if (c2(i)%len /= 10) error stop 17_4
    end do

    do i = 1, 50
        if (c3(i) /= 't'//achar(101-i)) error stop 18_4
    end do

    do i = 1, 100
        if (c4(i) /= 't'//achar(mod(99+i, 128))) error stop 19_4
    end do

    contains

    function genCharArray (length, size, lb)
        integer, intent(in) :: length, size, lb

        character(length) genCharArray (lb:lb+size-1)

        do i = lb, lb+size-1
            genCharArray(i) = 't'//achar(mod(abs(i), 128))
        end do
    end function

    function genComplxArray (start, size, lb)
        integer, intent(in) :: size, lb
        complex, intent(in) :: start

        complex genComplxArray(lb:lb+size-1)

        do i = lb, lb+size-1
            genComplxArray (i) = start + cmplx(i, i)
        end do
    end function
end
