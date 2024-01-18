!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 12/20/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.5.6.1: inheritance)
!                               Case: Test the parameterized poly-pointer
!                               components.  A linked list.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type dataType (k)
        integer, kind :: k

        integer(k) :: id
    end type

    type container (k)
        integer, kind :: k

        class(dataType(k)), pointer :: data => null()
        type(container(k)), pointer :: next => null()
    end type

    interface push_back
        module procedure push_back4
        module procedure push_back8
    end interface

    interface printListID
        module procedure printListID4
        module procedure printListID8
    end interface

    contains

    subroutine push_back4 (list, data)
        type(container(4)), intent (inout), target :: list
        class (dataType(4)), intent(in) :: data

        type (container(4)), pointer, save :: tail

        if (.not. associated(list%data)) then   !<-- list is empty
            allocate (list%data, source=data)

            tail => list
        else
            allocate (tail%next)

            tail => tail%next

            allocate (tail%data, source=data)
        end if
    end subroutine

    subroutine push_back8 (list, data)
        type(container(8)), intent (inout), target :: list
        class (dataType(8)), intent(in) :: data

        type (container(8)), pointer, save :: tail

        if (.not. associated(list%data)) then   !<-- list is empty
            allocate (list%data, source=data)

            tail => list
        else
            allocate (tail%next)

            tail => tail%next

            allocate (tail%data, source=data)
        end if
    end subroutine

    subroutine printListID4 (list)
        type (container(4)), target, intent(in) :: list

        type (container(4)), pointer :: iterator

        iterator => list

        do while (associated (iterator))
            if (.not. associated(iterator%data)) exit

            print *, iterator%data%id

            iterator => iterator%next
        end do
    end subroutine

    subroutine printListID8 (list)
        type (container(8)), target, intent(in) :: list

        type (container(8)), pointer :: iterator

        iterator => list

        do while (associated (iterator))
            if (.not. associated(iterator%data)) exit

            print *, iterator%data%id

            iterator => iterator%next
        end do
    end subroutine
end module

module m1
use m, only: dataType
    type, extends(dataType) :: mData (l)
        integer, len :: l

        character(l) :: name = 'default'
    end type
end module

program dtparamExtends031
use m
use m1
    type(container(4)), target :: dataList4
    type(container(8)), target :: dataList8

    type (dataType(8)) d8(10)
    type (dataType(4)) d4(6)

    type (mData(8, 10)) md8(5)
    type (mData(4, 6)) md4(9)

    if (any (md4%name /= 'defaul')) error stop 1_4

    d8%id = (/(i+80, i=0,9)/)
    d4%id = (/(i+40, i=0,5)/)

    md8%id = (/(800+i, i=1, 5)/)
    md4%id = (/(400+i, i=1, 9)/)

    !! now push these data into 2 lists

    do i = 1, 10
        call push_back(dataList8, d8(i))
    end do

    do i = 1, 6
        call push_back(dataList4, d4(i))
    end do

    do i = 1, 5
        call push_back(dataList8, md8(i))
    end do

    do i = 1, 9
        call push_back(dataList4, md4(i))
    end do

    !! now print the ids
    print *, 'list of IDs for kind 4'

    call printListID (dataList4)

    print *, 'list of IDs for kind 8'

    call printListID (dataList8)
end
