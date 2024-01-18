!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fext034.f
! %VERIFY: fext034.out:fext034.vf
! %STDIN:
! %STDOUT: fext034.out
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
        class (dataType), target, intent (inout) :: data_in

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

end module

program fext034
use m

    type (container), target :: dataList
    type (container), pointer :: node_ptr => null()

    type (dataType), target :: m1, m2, m3, m4

    m1%id = 5
    m2%id = 10
    m3%id = 100
    m4%id = 200


    call push_back(dataList, m1)
    call push_back(dataList, m2)
    call push_back(dataList, m3)
    call push_back(dataList, m4)

    ! now verify that m1, m2 and m3 are in the list
    node_ptr => dataList

    if (.not. associated (node_ptr%v_ptr, m1)) error stop 1_4
    print *, node_ptr%v_ptr%id  !! this is equivalent to print *, m1

    node_ptr => node_ptr%next

    if ((.not. associated (node_ptr)) .or. (.not. associated &
            (node_ptr%v_ptr, m2))) error stop 2_4

    print *, node_ptr%v_ptr%id !! print *, m2

    node_ptr => node_ptr%next

    if ((.not. associated (node_ptr)) .or. (.not. associated &
            (node_ptr%v_ptr, m3))) error stop 3_4

    print *, node_ptr%v_ptr%id !! print *, m3

    node_ptr => node_ptr%next

    if ((.not. associated (node_ptr)) .or. (.not. associated (node_ptr%v_ptr, m4))) error stop 4_4
    print *, node_ptr%v_ptr%id !! print *, m4

!    node_ptr => node_ptr%next

    if (associated (node_ptr%next)) error stop 5_4
end
