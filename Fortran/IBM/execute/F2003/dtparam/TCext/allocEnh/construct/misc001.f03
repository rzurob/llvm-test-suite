! GB DTP extension using:
! ftcx_dtp -qk -qnol -qnodefaultpv /tstdev/F2003/allocEnh/construct/misc001.f
! opt variations: -qnok -ql -qdefaultpv -qreuse=self

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/12/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Test a deep copy routine for a linked-list.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type dataType(k1)    ! (4)
        integer, kind         :: k1
        real(k1), allocatable :: data(:)

        contains

        procedure :: print => printDataTypeWithPrecision
    end type

    type node(k2)    ! (4)
        integer, kind                   :: k2
        type(dataType(k2)), allocatable :: data
        type(node(k2)), pointer         :: next => null()

        contains

        procedure :: print => printNodesWithPrecision
        procedure :: copy => copyNode
        generic :: assignment(=) => copy
        final :: finalizeNode
    end type

    contains

    subroutine printDataTypeWithPrecision (d)
        class(dataType(4)), intent(in) :: d

        if (allocated(d%data)) then
            write (*,'(6g12.4)') d%data
        else
            write (*, *) 'data has no value'
        end if
    end subroutine

    subroutine printNodesWithPrecision (n)
        class(node(4)), intent(in) :: n

        type(node(4)), pointer :: iterator

        if (allocated(n%data)) then
            print *, 'Node:'
            call n%data%print
        else
            write (*, *) 'node with no data'
        end if

        iterator => n%next

        do while (associated(iterator))
            if (allocated(iterator%data)) then
                print *, 'Node:'

                call iterator%data%print
            else
                write (*, *) 'node with no data'
            end if

            iterator => iterator%next
        end do
    end subroutine

    recursive subroutine copyNode (n1, n2)
        class(node(4)), intent(inout) :: n1
        type(node(4)), intent(in) :: n2

        if (allocated(n2%data)) n1%data = n2%data

        if (associated(n2%next)) then
            allocate(n1%next)

            n1%next = n2%next
        end if
    end subroutine

    recursive subroutine finalizeNode (n)
        type(node(4)), intent(inout) :: n

        if (allocated(n%data)) deallocate(n%data)

        if (associated(n%next)) deallocate(n%next)
    end subroutine
end module

program misc001
use m
    type(node(4)), pointer :: n1
    type(node(4)), allocatable :: n2

    allocate(n1)

    n1%data = dataType(4)((/1/))

    allocate (n1%next)

    n1%next%data = dataType(4)((/1,2/))

    allocate(n1%next%next)

    n1%next%next%data = dataType(4)((/1,2,3/))

    allocate (n1%next%next%next)

    n1%next%next%next%data = dataType(4)(null())


    !! do the deep copy
    allocate (n2)
    n2 = n1

    call n2%print

    if (associated(n2%next, n1%next) .or. &
        associated(n2%next%next, n1%next%next) .or. &
        associated(n2%next%next%next, n1%next%next%next)) error stop 1_4
end