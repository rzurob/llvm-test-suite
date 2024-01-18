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
!*  DATE                       : 02/10/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.5.8: derived-type-spec)
!                               Case: Still the default type parameters for the
!                               parameterized component; a linked-list.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type dataType (k, n)
        integer, kind :: k
        integer, len :: n

        real(k) :: data(n) = 0.0d0
    end type

    type node (k, n)
        integer, kind :: k = 8
        integer, len :: n = 33

        type(dataType(k,n)) :: data
        type (node(k,n)), pointer :: next => null()
    end type

    type listType
        type (node), pointer :: tail => null()
        type (node), pointer :: next => null()
    end type
end module

module linkedList
use m
    contains

    subroutine addNode (l, d)
        class(listType), intent(inout) :: l
        type (dataType(8, l%next%n)), intent(in) :: d

        type (node), pointer :: temp

        if (.not. associated(l%next)) then !!<-- empty list
            allocate(l%tail)

            l%tail%data = d

            l%next => l%tail
        else
            allocate (temp)

            temp%data = d

            l%tail%next => temp
            l%tail => temp
        end if
    end subroutine

    subroutine printList (l)
        class(listType), intent(in) :: l

        type(node), pointer :: iterator

        iterator => l%next

        do while (associated(iterator))
            write (*, 100) iterator%data%data

            iterator => iterator%next
        end do

100 format (5g15.7)
    end subroutine
end module

program dtparamDefVal004a
use linkedList
    type (listType) list

    type (dataType(8,:)), allocatable :: d1(:)

    allocate (dataType(8,33) :: d1(5))

    do i = 1, 5
        d1(i)%data = (/(i*1.0d2+j, j=1,33)/)
    end do

    do i = 5, 1, -1
        call addNode(list, d1(i))
    end do

    call printList(list)
end
