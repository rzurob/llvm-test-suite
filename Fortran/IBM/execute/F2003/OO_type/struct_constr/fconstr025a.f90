! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/22/2003
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (polymorphic pointer
!*                               initialization)
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

    type, extends(dataType) :: childData
        integer*4 :: id
    end type

    type, extends(childData) :: mData
        character*20 :: name
    end type

    type (childData), target :: cd1_m
    type (mData), target :: md1_m = mData (name='md1_m', id = 10)
    type (base), save :: b1_m, b2_m
end module

program fconstr025a
use m1

    type (base) :: b1, b2
    type (mData), target :: md1 = mData (1, 'md1')
    type (childData), target :: cd1 = childData (id = 100)

    class (dataType), pointer :: d_ptr

    d_ptr => md1
    b1 = base (d_ptr)


    d_ptr => cd1
    b2 = base (d_ptr)


    d_ptr => cd1_m
    b1_m = base (d_ptr)


    d_ptr => md1_m
    b2_m = base (d_ptr)


    ! validate data b1, b2, b1_m and b2_m
    if (.not. associated (b1%data, md1)) error stop 1_4

    if (.not. associated (b2%data, cd1)) error stop 2_4

    if (.not. associated (b1_m%data, cd1_m)) error stop 3_4

    if (.not. associated (b2_m%data, md1_m)) error stop 4_4
end
