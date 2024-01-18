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
!*  DATE                       : 02/16/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.5.8: derived-type-spec)
!                               Case: Use of init-expr for the length type
!                               parameter values in a linked list.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type dataType (k, n)
        integer, kind :: k
        integer, len :: n

        real(k) :: data(n)

        contains

        procedure :: normalize8 => normalizeData8
        procedure :: getVal8 => getData8
    end type

    contains

    subroutine normalizeData8 (d1)
        class(dataType(k=8, n=*)), intent(inout) :: d1

        real(8) :: localSum

        localSum = sum(abs(d1%data))

        if (localSum >= 1.0d-8) then
            d1%data = d1%data /localSum
        end if
    end subroutine

    real(8) function getData8 (d1, i)
        class(dataType(k=8, n=*)), intent(in) :: d1
        integer, intent(in) :: i

        if ((i < 1) .or. (i > d1%n)) stop 20

        getData8 = d1%data(i)
    end function
end module

module nodeDef
use m
    type node(k, n)
        integer, kind :: k, n

        type (dataType(k=k, n=n)), pointer :: data => null()
        class(node(k=k, n=n)), pointer :: next => null()
    end type
end module

module list8_100
use nodeDef
    class(node(k=8, n=100)), pointer :: iterator

    private iterator

    contains

    subroutine addNode (l, d)
        type(node(k=8, n=100)), intent(inout), target :: l
        type(dataType(k=8, n=100)), intent(in) :: d

        class(node(k=8, n=100)), pointer :: tail

        if (.not. associated(l%data)) then
            allocate(l%data, source=d)
        else
            tail => l

            do while(associated(tail%next))
                tail => tail%next
            end do

            allocate (node(k=8, n=100) :: tail%next)
            allocate (tail%next%data, source=d)
        end if
    end subroutine

    subroutine begin (l)
        type (node(k=8, n=100)), intent(in), target :: l

        iterator => l
    end subroutine

    subroutine next()
        iterator => iterator%next
    end subroutine

    logical function hasMore()
        hasMore = associated(iterator%next)
    end function

    subroutine normalize (l)
        type (node(k=8, n=100)), intent(in), target :: l

        call begin(l)

        do while (hasMore())
            if (.not. associated(iterator%data)) stop 10

            call iterator%data%normalize8

            call next()
        end do
    end subroutine

    real(8) function getVal (i)
        integer, intent(in) :: i

        if (.not. associated(iterator%data)) stop 30

        getVal = iterator%data%getVal8(i)
    end function
end module

program dtparamKeyword003
use m
use nodeDef
use list8_100
    class(node(k=8, n=100)), pointer :: list
    type (dataType(k=8, n=100)), allocatable :: d1

    logical(4), external :: precision_r8
    real(8) temp

    allocate (node(k=8, n=100) :: list)
    allocate (dataType(k=8, n=100) :: d1)

    do i = 1, 20
        d1%data = (/(i*1.0d2+j, j=0, 99)/)

        call addNode (list, d1)
    end do

    call normalize(list)

    !! verify data
    call begin (list)

    i = 1

    do while (hasMore())
        temp = i*1.0d2*100 + 99*100/2

        do j = 1, 100
            if (.not. precision_r8 ((i*1.0d2+j-1)/temp, getVal(j))) &
                    error stop 1_4
        end do

        call next
        i = i + 1
    end do

    if (i /= 20) error stop 2_4
end
