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
!*  DATE                       : 02/09/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.5.8: derived-type-spec)
!                               Case: Use the deferred type parameter for the
!                               node structure.
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
    end type

    type node (k, n)
        integer, kind :: k
        integer, len :: n

        type (dataType(k=k, n=n)) :: data
        type (node(k=k, n=:)), pointer :: next => null(), head => null()
    end type
end module

module linkedList4
use m
    type (node(k=4, n=:)), pointer :: iterator => null()
    private iterator

    contains

    subroutine push2Front (n, d)
        type (node(k=4, n=*)), target, intent(inout) :: n
        type(dataType(k=4, n=n%n)), intent(in) :: d

        if (.not. associated(n%head)) then !<-- must be an empty list
            n%data = d
            n%head => n
        else        !<-- then we push the data as the front node
            allocate (node(k=4,n=n%n) :: n%head)

            n%head%data = n%data

            n%head%next => n%next
            n%next => n%head

            n%data = d
        end if
    end subroutine

    subroutine begin (n)
        type (node(k=4, n=*)), target, intent(in) :: n

        iterator => n
    end subroutine

    logical function hasMore ()
        hasMore = associated(iterator) .and. associated(iterator%next)
    end function

    subroutine next ()
        iterator => iterator%next
    end subroutine

    function getVal ()
        type (node(k=4, n=:)), pointer :: getVal

        getVal => iterator
    end function
end module

program dtparamKeyword002a
use m
use linkedList4
    type (node(k=4, n=:)), pointer :: list, localNode

    type(dataType(k=4, n=20)), allocatable :: d1(:)

    logical(4), external :: precision_r4

    allocate (node(k=4, n=20) :: list)
    allocate (d1(35))

    !! assign values to d1
    do i = 1, 35
        d1(i)%data = (/(i*1.0e2+j, j=1,20)/)
    end do

    !! push d1 into list by push2Front
    do i = 1, 35
        call push2Front (list, d1(i))
    end do

    !! verify the results
    call begin (list)

    icount = 1

1000  localNode => getVal()

    if (.not. associated(localNode)) error stop 1_4

    do i = 1, 20
        if (.not. precision_r4(localNode%data%data(i), (36-icount)*1.0e2+i)) &
                error stop 2_4
    end do

    if (hasMore()) then
        icount = icount + 1
        call next()
        goto 1000
    end if

    if (icount /= 35) error stop 4_4
end
