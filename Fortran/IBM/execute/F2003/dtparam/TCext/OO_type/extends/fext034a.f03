! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 11, 2003
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : type extension (polymorphic type components)
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
    type dataType(k2)
        integer, kind :: k2
        integer(k2) :: id
    end type

    type container(k1)
        integer, kind :: k1
        type (container(k1)), pointer :: next => null ()
        class (dataType(k1)), pointer :: v_ptr => null ()
    end type

    contains

    ! this subroutine push the node object into the list
    ! NOTE: due to the fact that allocate is not working properly yet
    ! The data object is pointer associated by the v_ptr
    subroutine push_back (list, data_in)
        type (container(4)), intent (inout) :: list
        class (dataType(4)), target, intent (in) :: data_in

        type (container(4)), pointer :: currentNode
        type (container(4)), pointer :: newNode

        ! if the list is empty
        if (.not. associated(list%v_ptr)) then
            list%v_ptr=>data_in

        ! there is only one element in the list
        else if (.not. associated(list%next)) then
            allocate (newNode)

            newNode%v_ptr => data_in
            newNode%next => null()
            list%next => newNode
        else

            currentNode => list%next

            do while (associated (currentNode) .and. associated (currentNode%next))
                currentNode => currentNode%next
            end do

            allocate (currentNode%next)

            currentNode%next%next=> null()
            currentNode%next%v_ptr=>data_in

        end if
    end subroutine

    subroutine printList (list)
        type (container(4)), target, intent(in) :: list
        type (container(4)), pointer :: iterator

        iterator => list

        do while (associated(iterator))
            if (.not. associated(iterator%v_ptr)) exit

            print *, iterator%v_ptr%id

            iterator => iterator%next
        end do
    end subroutine
end module

program fext034a
use m

    type (container(4)), target :: dataList

    type, extends (dataType) :: mainData(k3)
        integer, kind :: k3
        real(k3) :: value = 1.0
    end type

    type (mainData(4,4)), target :: m1, m2, m3, m4

    m1%id = 5
    m2%id = 10
    m3%id = 100
    m4%id = 200


    call push_back(dataList, m1)
    call push_back(dataList, m2)
    call push_back(dataList, m3)
    call push_back(dataList, m4)

    ! now verify that m1, m2, m3 and m4 are in the list

    call printList (dataList)
end
