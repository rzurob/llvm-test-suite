! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/23/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (pointer assigned to
!*                               function return result)
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
        contains

        procedure, pass :: print => printData
    end type

    private printData
    contains

    subroutine printData (d)
        class (dataType), intent(in) :: d

        print *, 'dataType, empty type'
    end subroutine
end module

module m1
use m

    type, extends(dataType) :: mData
        integer*4 :: id

        contains

        procedure, pass :: print => printmData
    end type

    type, extends(mData) :: nData
        character*20 :: name = 'no-name'

        contains

        procedure, pass :: print => printnData
    end type

    contains

    subroutine printmData (d)
        class (mData), intent(in) :: d

        print *, 'id = ', d%id
    end subroutine

    subroutine printnData (d)
        class (nData), intent(in) :: d

        print *, 'id = ', d%id, '; name = ', d%name
    end subroutine
end module

program fpAssgn004a3
use m1

    interface makeData
        !! l is the starting id; n is the size of the array
        function createmData (l, n)
        use m1
            type (mData), pointer :: createmData(:)

            integer*4, intent(in) :: l, n
        end function

        !! l is the starting id; n is the size of the array
        function createnData (l, c, n)
        use m1
            type(nData), pointer :: createnData(:)
            integer*4, intent(in) :: l, n
            character(*), intent(in) :: c
        end function

        !! n is the size of the array
        function createDataType(n)
        use m
            type(dataType), pointer :: createDataType(:)
            integer*4, intent(in) :: n
        end function

    end interface

    interface
        subroutine printData (d)
        use m1
            class(dataType), pointer :: d(:)
        end subroutine
    end interface

    class (dataType), pointer :: d_ptr(:)

    !! make an array of mData with 10 elements, id starting from 2
    d_ptr => makeData (2, 10)

    call printData(d_ptr)

    !! make an array of nData with 5 elements, id starting from 0
    d_ptr => makeData (0, 'nData', 5)

    call printData(d_ptr)

end

!! this function calls allocate to create an mData object
function createmData (l, n)
use m1
    type (mData), pointer :: createmData(:)
    integer*4, intent(in) :: l, n

    allocate (createmData(n))

    do i = 1, n
        createmData(i) = mData (i+l-1)
    end do
end function


!! this function calls allocate to create an nData object
function createnData (l, c, n)
use m1
    type (nData), pointer :: createnData(:)
    integer*4, intent(in) :: l, n
    character(*), intent(in) :: c

    allocate (createnData(n))

    createnData = (/(nData(i+l-1, c), i=1,n)/)
end function


!! this function calls allocate to create a dataType object
function createDataType (n)
use m
    type(dataType), pointer :: createDataType(:)
    integer*4, intent(in) :: n

    allocate (createDataType(n))

end function

subroutine printData (d)
use m
    class(dataType), pointer :: d(:)

    do i = 1, size(d)
        call d(i)%print
    end do

    deallocate(d)
end subroutine
