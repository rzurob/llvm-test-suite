! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/22/2003
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (polymorphic pointer
!*                               components initialization)
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
    type dataType
    end type
end module

module m1
use m
    type base
        class (dataType), pointer :: data => null()
    end type

    type, extends(base) :: child
        character (20) :: name
    end type

    type, extends(dataType) :: moduleData
        integer*4 :: id
    end type

    type (child), save :: c1_m, c2_m
    type (base), save :: b1_m, b2_m
    type (moduleData), target :: m1_m = moduleData (1)
    type (moduleData), target :: m2_m
    type (dataType), target :: d1_m

    contains

    subroutine initializeModData
        b1_m = base (m1_m)

        m2_m = moduleData(id = 2)
        b2_m = base (data = m2_m)

        c1_m = child (m1_m, 'c1_m')

        d1_m = dataType()
        c2_m = child (name = 'c2_m', data = d1_m)
    end subroutine
end module

program fconstr025
use m1

    type, extends(dataType) :: mainData
        integer*8 :: value
    end type

    type (mainData), target :: md1 = mainData (100)

    type (base) :: b1, b2
    type (child) :: c1, c2

    type (mainData), target :: md10 = mainData (5)
    type (mainData), target :: md11 = mainData (-1)

    ! when the program started, all the pointer components are dissassociated
    ! for b1, b2, c1, c2, b1_m, b2_m, c1_m and c2_m

    if (associated (b1%data) .or. associated (b2%data) .or. &
        associated (c1%data) .or. associated (c2%data) .or. &
        associated (b1_m%data) .or. associated (b2_m%data) .or. &
        associated (c2_m%data) .or. associated (c2_m%data) ) &
                error stop 20_4

    ! initialize all data variables

    b1 = base (md10)
    b2 = base (data = m1_m)

    c1 = child (data = md1, name = 'c1')
    c2 = child (name = 'c2', data = md11)

    call initializeModData

    ! validate all the main program variables b1, b2, c1 and c2
    if (.not. associated (b1%data, md10)) error stop 1_4

    if (.not. associated (b2%data, m1_m)) error stop 2_4

    if ((.not. associated (c1%data, md1)) .or. (c1%name /= 'c1')) error stop 3_4

    if ((.not. associated (c2%data,md11)) .or. (c2%name /= 'c2')) error stop 4_4


    ! validate the module variables: b1_m, b2_m, c1_m and c2_m
    if (.not. associated (b1_m%data, m1_m)) error stop 5_4

    if (.not. associated (b2_m%data, m2_m)) error stop 6_4

    if ((.not. associated(c1_m%data,m1_m)) .or. (c1_m%name/='c1_m')) error stop 7_4

    if ((.not. associated(c2_m%data)).or.(c2_m%name /= 'c2_m')) error stop 8_4

end
