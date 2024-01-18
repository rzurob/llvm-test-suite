!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fconstr029.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/23/2003
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (polymorphic pointer
!*                               array component)
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        class (childData), pointer :: value(:) => null()
    end type

    type (base), save :: b1_m(3), b2_m

    type dataType
    end type

    type, extends(dataType) :: childData
        integer*4 :: id
    end type

    type, extends(childData) :: mData
        character*20 :: name
    end type

    class (childData), pointer :: cd1_mptr(:) => null()
    type (childData), target :: cd1_m(10)
    type (mData), target :: md1_m(20)

    contains

    subroutine initializeModuleData
        cd1_m = (/(childData (i), i=11,20)/)

        md1_m = (/(mData(i, name='md1_m'), i=101, 120)/)

        b1_m = base (value = cd1_m)

        b2_m = base (cd1_mptr)
        b2_m = base (value = md1_m)
    end subroutine
end module

program fconstr029
use m

    type, extends(base) :: child
        character(20) :: name
    end type

    type (base) :: b1 = base (null())
    type (base) :: b2

    type (child) :: c1

    type (childData), target :: cd1(5)
    type (mData), target :: md1(6)

    ! at this point, all pointer components are nulls
    if (associated (b1%value) .or. associated (b2%value) .or. &
        associated (b1_m(1)%value) .or. associated (b1_m(2)%value) .or. &
        associated (b1_m(3)%value) .or. associated (b2_m%value) .or. &
        associated (c1%value)) error stop 20_4

    call initializeModuleData

    cd1 = (/(childData(i+1), i=1,5)/)
    md1 = (/(mData(2*i, 'md1'), i=31,36)/)


    b1 = base (md1)
    b2 = base (value = cd1)

    c1 = child (base = base (value = cd1), name = 'c1')

    ! validate all the data: b1, b2, b1_m, b2_m and c1

    if ((.not. associated(b1%value, md1)) .or. (size(b1%value) /= 6)) error stop 1_4

    do i = 1,6
        if (b1%value(i)%id /= 2*i+60) error stop 2_4
    end do


    if ((.not. associated (b2%value,cd1)) .or. (size(b2%value) /= 5)) error stop 3_4

    do i = 1, 5
        if (b2%value(i)%id /= i+1) error stop 4_4
    end do


    if (.not. associated (c1%value, cd1) .or. (c1%name /= 'c1')) error stop 5_4
    if (size(c1%value) /= size(cd1)) error stop 6_4


    do i = 1, size(b1_m)
        if ((.not. associated (b1_m(i)%value, cd1_m)) .or. &
            (size(b1_m(i)%value) /= 10)) error stop 7_4

        do j = 1, 10
            if (b1_m(i)%value(j)%id /= 10+j) error stop 8_4
        end do
    end do


    if ((.not. associated (b2_m%value, md1_m)) .or. &
        (size(b2_m%value) /= 20)) error stop 9_4

    do i = 1, 20
        if (b2_m%value(i)%id /= 100+i) error stop 10_4
    end do


    !! reset some of the variables
    nullify (cd1_mptr)

    c1 = child (cd1_mptr, 'c1')
    if (associated (c1%value)) error stop 11_4

    cd1_mptr => md1_m
    c1 = child (cd1_mptr, 'c1')

    if (.not. associated (c1%value, md1_m)) error stop 12_4
    if (size (c1%value) /= 20) error stop 13_4

    do i = 101, 120
        if (c1%value(i-100)%id /= i) error stop 14_4
    end do
end
