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
!*  DATE                       : 03/01/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : specific type bound (linked-list binding; copy
!                               operation)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type, private :: node
        class(*), allocatable :: data

        type(node), pointer :: next => null()

        contains

        final :: finalizeNode
    end type

    type list
        type (node), pointer, private :: head => null()
        type (node), pointer, private :: current => null()
        type (node), pointer, private :: tail => null()

        contains

        procedure :: push_back => pushDataToEnd
        procedure :: begin => startIterator
        procedure :: end => endIterator
        procedure :: next => move2NextNode
        procedure :: getVal => getCurrentNodeVal
        procedure :: copy => copyL2toL1
        procedure :: clear => clearList
        final :: finalizeList
    end type


    contains

    recursive subroutine finalizeNode (n)
        type (node), intent(inout) :: n
        if (associated (n%next)) deallocate (n%next)
    end subroutine

    subroutine finalizeList (l)
        type (list), intent(inout) :: l

        nullify (l%current, l%tail)

        if (associated (l%head)) deallocate (l%head)
    end subroutine


    class (*) function getCurrentNodeVal (l)
        allocatable getCurrentNodeVal
        class (list), intent(in) :: l

        if (associated (l%current)) then
            allocate (getCurrentNodeVal, source=l%current%data)
        else
            print *, 'no data selected'
            error stop 10_4
        end if
    end function

    subroutine pushDataToEnd (l, x)
        class (list), intent(inout) :: l
        class (*), intent(in) :: x

        if (.not. associated (l%head)) then !! very first node
            allocate (l%head)
            allocate (l%head%data, source=x)

            l%tail => l%head
        else
            allocate (l%tail%next, source=node(data=x))

            l%tail => l%tail%next
        end if
    end subroutine

    subroutine startIterator (l)
        class (list), intent(inout) :: l

        l%current => l%head
    end subroutine

    logical function endIterator (l)
        class (list), intent(in) :: l

        endIterator = .not. associated (l%current)
    end function

    subroutine move2NextNode (l)
        class (list), intent(inout) :: l

        if (associated (l%current)) l%current => l%current%next
    end subroutine


    subroutine copyL2toL1 (l1, l2)
        class (list), intent(out) :: l1
        class (list), intent(in) :: l2

        type (node), pointer :: iterator

        !! set up the head in l1
        if (associated (l2%head)) then
            allocate (l1%head, source=node(l2%head%data))

            l1%tail => l1%head
            iterator => l2%head%next
        end if

        !! allocate the rest nodes
        do while (associated (iterator))
            allocate (l1%tail%next, source=node(iterator%data))

            l1%tail => l1%tail%next
            iterator => iterator%next
        end do
    end subroutine

    subroutine clearList (l)
        class (list), intent(inout) :: l

        call finalizeList (l)
    end subroutine
end module

program ftpbnd512a
use m
    type (list) :: l1, l2

    call l1%push_back (300)
    call l1%push_back (4.05_8)
    call l1%push_back ('test 101')
    call l1%push_back ((1.02e0_8, 2.11e0_8))
    call l1%push_back (3.1 == 10.2)

    call l2%push_back (1.0)

    call l2%copy (l1)

    !! print Values

    call l2%begin

    do while (.not. l2%end())
        call printX (l2%getVal())

        call l2%next
    end do


    contains

    subroutine printX (x)
        class (*), intent(in) :: x

        select type (x)
            type is (integer)
                print *, x
            type is (real(8))
                write (*, '(f12.2)') x
            type is (complex(8))
                write (*, '(2(f10.2))') x
            type is (character(*))
                print *, x
            type is (logical)
                print *, x
            class default
                error stop 20_4
        end select
    end subroutine
end
