! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assgnment (a realistic example of
!*                               linked-list)
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

        procedure :: print => printDataType
    end type

    type, extends(dataType) :: moduleData
        integer*4 :: id = 0

        contains

        procedure :: print => printmoduleData
    end type

    contains

    subroutine printDataType (d)
        class (dataType), intent(in) :: d

        print *, 'empty type; do NOT reference'
    end subroutine

    subroutine printmoduleData (d)
        class (moduleData), intent(in) :: d

        print *, 'moduleData:', d%id
    end subroutine
end module

module m1
use m, only : dataType, moduleData

    type, private :: node
        type (node), pointer :: next => null()
        class (dataType), pointer :: data => null()
    end type

    type container
        type (node), pointer :: head => null()

        contains

        procedure :: print => printContainer
        procedure :: size => countNodes
        procedure :: push_back => addNode
        procedure :: clear => deleteAllNodes
    end type

    contains

    subroutine printContainer (co)
        class (container), intent(in), target :: co

        type (node), pointer :: iterator

        iterator => co%head

        do while (associated (iterator))
            if (associated (iterator%data)) call iterator%data%print

            iterator => iterator%next
        end do
    end subroutine

    integer*4 function countNodes (co)
        class (container), intent(in), target :: co

        type (node), pointer :: iterator

        countNodes = 0

        iterator => co%head

        do while (associated (iterator))
            countNodes = countNodes + 1
            iterator => iterator%next
        end do
    end function

    subroutine addNode (co, d1)
        class (container), intent(inout) :: co
        class (dataType), target :: d1

        type (node), pointer :: tail

        if (.not. associated (co%head)) then
            allocate (co%head)
            co%head = node (data = d1)
        else
            tail => co%head

            do while (associated (tail) .and. (associated (tail%next)))
                tail => tail%next
            end do

            allocate (tail%next)

            tail%next = node (data = d1)
        end if
    end subroutine

    subroutine deleteAllNodes (co)
        class (container), intent(inout) :: co

        type (node), pointer :: iterator, temp

        iterator => co%head

        do while (associated (iterator) .and. associated (iterator%next))
            temp => iterator%next

            deallocate (iterator)

            iterator => temp
        end do

        deallocate (iterator)
        nullify (co%head)
    end subroutine
end module

module m2
use m, only : dataType

    type, extends (dataType) :: mData
        logical*2 flag

        contains

        procedure :: print => printMdata
    end type

    type, extends (mData) :: nData
        character*20 :: name

        contains

        procedure :: print => printNdata
    end type

    contains

    subroutine printMdata (d)
        class (mData), intent(in) :: d

        print *, 'mData:', d%flag
    end subroutine

    subroutine printNdata (d)
        class (nData), intent(in) :: d

        print *, 'nData:', d%flag, d%name
    end subroutine
end module

program fpAssgn020
use m1
use m2

    type (container) list

    type (moduleData), target, save :: md1, md2
    type (mData), save, target :: md3
    type (nData), save, target :: md4

    type (nData), allocatable, target :: nd1

    class (dataType), pointer :: md5
    class (mData), pointer :: nd2

    type (mData), pointer :: md6

    md1 = moduleData (10)
    md2 = moduleData (20)

    md3 = mData (1==1)
    md4 = nData (1<3, 'md4')

    allocate (nd1)
    nd1 = nData (.true., 'nd1')

    md5 => nd1

    !! let's try a non-poly pointer to a poly target
    nd2 => nd1
    md6 => nd2

    call list%push_back (md1)
    call list%push_back (md2)
    call list%push_back (md3)
    call list%push_back (md4)
    call list%push_back (md5)
    call list%push_back (md6)

    call list%print

    if (list%size() /= 6) error stop 1_4

    call list%clear

    if (list%size() /= 0) error stop 2_4
end
