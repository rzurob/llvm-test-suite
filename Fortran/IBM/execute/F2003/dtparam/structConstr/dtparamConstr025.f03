! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/07/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.9: structure constructor)
!                               Case: Use of component keyword to assign data
!                               component of a parameterized derived type.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, n)
        integer, kind :: k
        integer, len :: n

        real(k) :: data(n)
        logical(k/4) :: flag
    end type

    type, extends(base) :: child (l)
        integer, len :: l

        integer(k/2) :: id
        character(l) :: name
    end type

    type, extends(child) :: gen3
        type(child(k*2, n, l+10)) :: ch
        complex(k) :: cx
    end type

    type (gen3(4,10,20)) :: g3_m = gen3(4,10,20)(data=1.0, flag=2>1_8, id=100,&
            name='g3_m module data', cx=(2.0, 1.0), ch=child(8,10,30)&
            (name='g3_m%ch sub-object',id=1,flag=.false.,data=(/(i, i=1,10)/)))
end module

program dtparamConstr025
use m
    real(8) :: d1(100)
    character(100) :: desc
    integer(8) :: iVal(10) = (/(i*1000, i=1, 10)/)

    logical(4), external :: precision_r4, precision_r8, precision_x8, &
                            precision_x6, precision_r6

    type (gen3(8,35,21)) :: g3

    d1 = log(sqrt((/(i*1.0d0, i=1,100)/)))

    desc = genString (10)

    g3 = gen3(8,35,21) (name=genString(2), id=iVal(7), data=d1(:35), &
        flag=.true., ch=child(16,35,31)(d1(1:35)+d1(60:94), .false., iVal(10), &
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
