! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/08/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.8: derived-type-spec)
!                               Case: Test on a linked-list.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type dataType(k, n)
        integer, kind :: k
        integer, len :: n

        real(kind=k) :: data(n)
    end type

    type node(k1, n)
        integer, kind :: k1
        integer, len :: n

        type(dataType(k=k1,n=n)) :: data
        type (node(k1=k1,n=n)), pointer :: next => null()
    end type

end module

module linkedList8
use m
    type (node(k1=8,n=:)), pointer :: iterator => null()
    private iterator

    contains

    subroutine addNode (n, d)
        type(node(k1=8, n=*)), intent(inout), target :: n
        type(dataType(k=8, n=n%n)), intent(in) :: d

        type (node(k1=8,n=:)), pointer :: iterator

        iterator => n

        !! find the tail of the list
        do while (associated(iterator%next))
            iterator => iterator%next
        end do

        allocate (node(k1=8,n=n%n) :: iterator%next)

        iterator%next%data = d
    end subroutine

    subroutine begin (n)
        type (node(k1=8, n=*)), intent(in), target :: n

        iterator => n
    end subroutine

    logical function hasMore (n)
        type (node(k1=8, n=*)), intent(in), target :: n

        hasMore = associated(iterator%next)
    end function

    subroutine next (n)
        type (node(k1=8, n=*)), intent(in), target :: n

        iterator => iterator%next
    end subroutine

    function getVal ()
        type (node(k1=8,n=:)), pointer :: getVal

        getVal => iterator
    end function
end module

program dtparamKeyword002
use m
use linkedList8
    logical(4), external :: precision_r8

    type (node(k1=8, n=:)), pointer :: list

    type (dataType(k=8, n=10)) :: d1(20)

    do i = 1, 20
        d1(i)%data = (/(i*1.0d1+j, j=0,9)/)
    end do

    allocate (node(k1=8,n=10) :: list)

    !! push d1 into list
    list%data = d1(1)

    do i = 2, 20
        call addNode(list, d1(i))
    end do

    !! iterate through the list and verify the results
    call begin (list)

    i = 1

10  associate( x => getVal())
        do j = 1, 10
            if (.not. precision_r8(x%data%data(j), i*1.0d1+j-1)) error stop 1_4
        end do
    end associate

    i = i + 1

    if (hasMore(list)) then
        call next(list)
        go to 10
    end if
end
