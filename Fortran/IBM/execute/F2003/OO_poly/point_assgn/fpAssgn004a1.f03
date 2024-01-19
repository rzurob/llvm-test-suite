! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/15/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : pointer assignment (unlimited poly-pointer
!*                               assigned to a function return result that of
!*                               a derived type)
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
        integer*4 :: id
    end type

    contains

    !! makeData allocates memory for the returned result if the input
    !! is not negative
    function makeData (i)
        type(dataType), pointer :: makeData
        integer*4, intent(in) :: i

        if (i >= 0) then
            allocate (makeData)
            makeData = dataType (id = i)
        else
            nullify (makeData)
        end if
    end function

    !! assgnData uses static memory address for the returned result
    function assgnData (i)
        type(dataType), pointer :: assgnData
        integer*4, intent(in) :: i

        type(dataType), save, target :: staticData = dataType(0)

        if (i >= 0) then
            assgnData => staticData
            staticData%id = i
        else
            assgnData => null()
        end if
    end function
end module

program fpAssgn004a1
use m

    class(*), pointer :: x
    type (dataType), pointer :: d_ptr
    class (dataType), pointer :: d_ptr2

    x => makeData (-10)

    if (associated(x)) error stop 1_4

    x => makeData (10)

    if (.not. associated(x)) error stop 2_4

    x => assgnData (-10)

    if (associated(x)) error stop 3_4

    x => assgnData (10)

    d_ptr => assgnData (1)

    if ((.not. associated(x)) .or. (.not. associated(x, d_ptr))) error stop 4_4

    d_ptr2 => assgnData (20)

    if (.not. associated(x, d_ptr2)) error stop 5_4

end
