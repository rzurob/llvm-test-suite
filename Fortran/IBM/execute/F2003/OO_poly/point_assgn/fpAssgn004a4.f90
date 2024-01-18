! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/23/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (poly pointer assigned
!*                               to function return results; arrays; test
!*                               bounds, size and dynamic types)
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
        integer*4 :: id

        contains

        procedure, nopass :: typeID => baseID
    end type

    type, extends (base) :: child
        character*20 :: name

        contains

        procedure, nopass :: typeID => childID
    end type

    contains

    integer*4 function baseID()
        baseID = 1
    end function

    integer*4 function childID ()
        childID = 2
    end function
end module


program fpAssgn004a4
use m
    interface makeData
        function baseArray (i)
        use m
            type(base), pointer :: baseArray(:)
            integer*4, intent(in) :: i
        end function

        function childArray (i, c)
        use m
            type(child), pointer :: childArray(:)
            integer*4, intent(in) :: i
            character(*), intent(in) :: c
        end function
    end interface

    class (base), pointer :: b_ptr(:)

    !! make an array of base type pointer
    b_ptr => makeData(10)

    if ((size(b_ptr) /= 21) .or. (lbound(b_ptr, 1) /= -10) &
        .or. (ubound(b_ptr, 1) /= 10)) error stop 1_4

    if (b_ptr%typeID() /= 1) error stop 2_4

    if (any (b_ptr%id /= 10)) error stop 3_4


    !! make a pointer array of child type
    b_ptr => makeData(-10, 'good')

    if ((size(b_ptr) /= 20) .or. (lbound(b_ptr, 1) /= 1) &
        .or. (ubound(b_ptr, 1) /= 20)) error stop 4_4


    if (b_ptr%typeID() /= 2) error stop 5_4

    if (any (b_ptr%id /= -10)) error stop 6_4

    deallocate (b_ptr)
end

function baseArray (i)
use m
    type(base), pointer :: baseArray(:)
    integer*4, intent(in) :: i

    type(base), static, target :: oneArray (-10:10)

    baseArray => oneArray
    baseArray = base (i)
end function

function childArray (i, c)
use m
    type(child), pointer :: childArray(:)
    integer*4, intent(in) :: i
    character(*), intent(in) :: c

    allocate (childArray(20))

    childArray = child (i, c)

end function
