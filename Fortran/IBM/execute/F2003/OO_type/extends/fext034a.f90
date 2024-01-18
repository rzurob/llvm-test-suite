!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fext034a.f
! %VERIFY: fext034a.out:fext034a.vf
! %STDIN:
! %STDOUT: fext034a.out
! %EXECARGS:
! %POSTCMD:
! %END
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
    type container
        type (container), pointer :: next => null ()
        class (dataType), pointer :: v_ptr => null ()
    end type

    type dataType
        integer*4 :: id
    end type

    contains

    ! this subroutine push the node object into the list
    ! NOTE: due to the fact that allocate is not working properly yet
    ! The data object is pointer associated by the v_ptr
    subroutine push_back (list, data_in)
        type (container), intent (inout) :: list
        class (dataType), target, intent (in) :: data_in

        type (container), pointer :: currentNode
        type (container), pointer :: newNode

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
        type (container), target, intent(in) :: list
        type (container), pointer :: iterator

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

    type (container), target :: dataList

    type, extends (dataType) :: mainData
        real*4 :: value = 1.0
    end type

    type (mainData), target :: m1, m2, m3, m4

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
