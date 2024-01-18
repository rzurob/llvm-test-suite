! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/19/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : pointer assignment (correct line/col numbers
!*                              shall be given in the error message)
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

program fpAssgn025d
use m
    class (mData), allocatable :: md1     !<-- md1 should have TARGET attribute
    type (mData), pointer :: md2, md3

    integer*2, target :: i1 = 100

    allocate (md1, md2, md3)

    md2 = mData (i1, id = 10)

    md3 = md2
!a lot of other things commented out

    md3 = mData (id = 1, data = md1)    !<-- here the compiler shall fail


    deallocate (md2, md3)
end

