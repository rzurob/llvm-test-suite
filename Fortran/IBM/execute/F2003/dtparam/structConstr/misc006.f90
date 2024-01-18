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
!*  DATE                       : 03/07/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : miscellaneous (defect 317035)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base4_10! (k, n)
        real(4) :: data(10)
        logical(4/4) :: flag
    end type

    type, extends(base4_10) :: child4_10_20! (l)
        integer(4/2) :: id
        character(20) :: name
    end type

    type base8_10! (k, n)
        real(8) :: data(10)
        logical(8/4) :: flag
    end type

    type, extends(base8_10) :: child8_10_30! (l)
        integer(8/2) :: id
        character(30) :: name
    end type

    type base8_35! (k, n)
        real(8) :: data(35)
        logical(8/4) :: flag
    end type

    type, extends(base8_35) :: child8_35_21! (l)
        integer(8/2) :: id
        character(21) :: name
    end type

    type base16_35! (k, n)
        real(16) :: data(35)
        logical(16/4) :: flag
    end type

    type, extends(base16_35) :: child16_35_31! (l)
        integer(16/2) :: id
        character(31) :: name
    end type

    type, extends(child4_10_20) :: gen3_4_10_20
        type(child8_10_30) :: ch
        complex(4) :: cx
    end type

    type, extends(child8_35_21) :: gen3_8_35_21
        type(child16_35_31) :: ch
        complex(8) :: cx
    end type

    type (gen3_4_10_20) :: g3_m = gen3_4_10_20(data=1.0, flag=2>1_8, id=100,&
            name='g3_m module data', cx=(2.0, 1.0), ch=child8_10_30&
            (name='g3_m%ch sub-object',id=1,flag=.false.,data=(/(i, i=1,10)/)))
end module

program dtparamConstr025
use m
    real(8) :: d1(100)
    character(100) :: desc
    integer(8) :: iVal(10) = (/(i*1000, i=1, 10)/)

    logical(4), external :: precision_r4, precision_r8, precision_x8, &
                            precision_x6, precision_r6

    type (gen3_8_35_21) :: g3

    d1 = log(sqrt((/(i*1.0d0, i=1,100)/)))

    desc = genString (10)

    g3 = gen3_8_35_21 (name=genString(2), id=iVal(7), data=d1(:35), &
        flag=.true., ch=child16_35_31(d1(1:35)+d1(60:94), .false., iVal(10), &
        name=genString(4)), cx=cmplx(d1(55), d1(65), 8))


    !! verify results
    do i = 1, 10
        if (.not. precision_r4(g3_m%data(i), 1.0)) error stop 1_4

        if (.not. precision_r8(g3_m%ch%data(i), real(i, 8))) error stop 2_4
    end do

    if (.not. g3_m%flag) error stop 3_4
    if (g3_m%id /= 100) error stop 4_4
    if (g3_m%name /= 'g3_m module data') error stop 5_4
    if (.not. precision_x8(g3_m%cx, (2.0, 1.0))) error stop 6_4

    if ((g3_m%ch%id /= 1) .or. (g3_m%ch%name /= 'g3_m%ch sub-object') .or. &
        (g3_m%ch%flag)) error stop 7_4



    do i = 1, 35
        if (.not. precision_r8(g3%data(i), log(i*1.0d0)/2.0d0)) error stop 8_4

        if (.not. precision_r8(real(g3%ch%data(i), 8), log(i*(i+59)*1.0d0)/2.0d0))&
            error stop 9_4
    end do

    if (.not. g3%flag) error stop 10_4
    if (g3%id /= 7000) error stop 11_4
    if (g3%name /= desc(1:20)) error stop 12_4
    if (.not. precision_x6(g3%cx, cmplx(d1(55), d1(65), 8))) error stop 13_4

    if ((g3%ch%id /= 10000) .or. (g3%ch%name /= desc(1:31)) .or. g3%ch%flag) &
            error stop 14_4

    contains

    recursive character(i*10) function genString (i)
        integer, intent(in) :: i

        if (i <= 0) then
            return
        else
            genString = genString(i-1) // repeat(char(ichar('A')+i-1), 10)
        end if
    end function
end
