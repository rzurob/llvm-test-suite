! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/30/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (pointer assignment
!*                               takes place during the intrinsic assignment for
!*                               the pointer component)
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
        class (*), pointer :: data => null()
    end type

    type, extends (dataType) :: mData
        integer*4 :: id
    end type
end module

program fpAssgn025a2
use m
    class (mData), target, allocatable :: md1
    type (mData), pointer :: md2, md3

    integer*2, target :: i1 = 100

    allocate (md1, md2, md3)

    md2 = mData (i1, id = 10)

    md3 = md2

    if (.not. associated (md3%data, i1)) error stop 1_4

    md1%dataType = md3%dataType

    if (.not. associated (md1%data, i1)) error stop 2_4

    md3 = mData (id = 1, data = md1)

    md2 = md3

    if (.not. associated (md2%data, md1)) error stop 3_4

    deallocate (md2, md3)
end
