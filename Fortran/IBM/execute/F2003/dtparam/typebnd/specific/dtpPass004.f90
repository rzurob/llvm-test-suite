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
!*  DATE                       :
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                :
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module list
    type dataType (k)
        integer, kind :: k

        complex(k) :: data
    end type

    type node (k)
        integer, kind :: k

        type(dataType(k)), pointer :: data => null()
        type(node(k)), pointer :: next => null()

        contains

        procedure :: insert8 => insertData8
        procedure :: print8 => printData8
        procedure :: pop8 => popData8
    end type

    contains

    subroutine insertData8 (n1, d1)
        class(node(8)), intent(inout), target :: n1
        type(dataType(8)), intent(in) :: d1

        type(node(8)), pointer :: lastNode

        !! find the last node
        lastNode => n1

        do while (associated(lastNode%next))
            lastNode => lastNode%next
        end do

        if (associated(lastNode%data)) then     !<-- lastNode contains data
            allocate (lastNode%next)

            allocate (lastNode%next%data, source=d1)
        else
            allocate (lastNode%data)

            lastNode%data = d1
        end if
    end subroutine

    subroutine printData8 (n1)
        class(node(8)), intent(inout), target :: n1

        type(node(8)), pointer :: iterator

        iterator => n1

        do while (associated(iterator))
            if (associated(iterator%data)) then
                write (*, '(g18.10, g18.10)') iterator%data
            else
                write (*, *) 'NULL'
            end if

            iterator => iterator%next
        end do
    end subroutine

    type(dataType(8)) function popData8 (n1)
        class(node(8)), intent(inout), target :: n1

        type(node(8)), pointer :: head2BPoped

        head2BPoped => n1

        if (.not. associated(head2BPoped%data)) then
            print *, 'No data available'

            stop 10
        else
            popData8 = head2BPoped%data

            deallocate (head2BPoped%data)

            if (associated(head2BPoped%next)) then 
                n1%data => head2BPoped%next%data
                n1%next => head2BPoped%next%next
            end if
        end if
    end function
end module

program dtpPass004
use list
    type(node(8)), pointer :: theList

    type(dataType(8)) d1, d2(3)

    allocate (theList)

    call theList%insert8(dataType(8)(1.0))

    call theList%insert8(dataType(8)(cmplx(2.1_8, sin(2.1_8), 8)))

    !! this node is added manually
    allocate(theList%next%next)

    call theList%print8

    d1%data = cmplx(cos(0.8d0), sin(0.8d0), 8)

    call theList%insert8(d1)

    call theList%print8

    !! now pop all data
    do i = 1, 3
        d2(i) = theList%pop8()
    end do

    write (*, '(6g18.10)') d2

    d1 = theList%pop8() !<-- this terminates the program
end
