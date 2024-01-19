! GB DTP extension using:
! ftcx_dtp -qck -qk -qnol -qnodeferredlp -qreuse=base /tstdev/OO_poly/point_assgn/fpAssgn004a2.f
! opt variations: -qnock -qnok -ql -qdeferredlp -qreuse=none

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/04/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (polymorphic pointer
!*                               assigned to expr, where expr refers to a function that
!*                               returns a pointer type compatible with the
!*                               pointer object; function call resolved to
!*                               one specific in generics)
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
    type dataType(k1)    ! (4)
        integer, kind :: k1
        contains

        procedure, pass :: print => printData
    end type

    contains

    subroutine printData (d)
        class (dataType(4)), intent(in) :: d

        print *, 'dataType, empty type'
    end subroutine
end module

module m1
use m

    type, extends(dataType) :: mData    ! (4)
        integer(k1) :: id

        contains

        procedure, pass :: print => printmData
    end type

    type, extends(mData) :: nData(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name = 'no-name'

        contains

        procedure, pass :: print => printnData
    end type

    contains

    subroutine printmData (d)
        class (mData(4)), intent(in) :: d

        print *, 'id = ', d%id
    end subroutine

    subroutine printnData (d)
        class (nData(4,1,*)), intent(in) :: d

        call printmData(d%mData)

        print *, 'name = ', d%name
    end subroutine
end module

program fpAssgn004a2
use m1

    interface makeData
        function createmData (i)
        use m1
            type (mData(4)), pointer :: createmData

            integer*4, intent(in) :: i
        end function

        function createnData (i, c)
        use m1
            type(nData(4,1,20)), pointer :: createnData
            integer*4, intent(in) :: i
            character(*), intent(in) :: c
        end function

        function createDataType()
        use m
            type(dataType(4)), pointer :: createDataType
        end function
    end interface

    class (dataType(4)), pointer :: d_ptr

    d_ptr => makeData ()

    call d_ptr%print

    deallocate (d_ptr)

    d_ptr => makeData (10)

    call d_ptr%print

    deallocate (d_ptr)

    d_ptr => makeData (100, 'd_ptr pointer')
    call d_ptr%print

    deallocate (d_ptr)

end

!! this function calls allocate to create an mData object
function createmData (i)
use m1
    type (mData(4)), pointer :: createmData
    integer*4, intent(in) :: i

    allocate (createmData)

    createmData = mData(4) (i)
end function


!! this function calls allocate to create an nData object
function createnData (i, c)
use m1
    type (nData(4,1,20)), pointer :: createnData
    integer*4, intent(in) :: i
    character(*), intent(in) :: c

    allocate (createnData)

    createnData = nData(4,1,20)(i, c)
end function


!! this function calls allocate to create a dataType object
function createDataType ()
use m
    type(dataType(4)), pointer :: createDataType

    allocate (createDataType)

end function

