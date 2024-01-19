! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/28/2005
!*
!*  DESCRIPTION                : specific type bound (a linked list; test the
!                               iteration using JAVA's methodology)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type, private :: node
        class(*), allocatable :: data

        type(node), pointer :: next => null()
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
    end type


    contains

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

end module

program ftpbnd512
use m
    type (list) :: l1

    call l1%push_back (100)
    call l1%push_back (1.05_8)
    call l1%push_back ('test 1')
    call l1%push_back ((1.02e0_8, 2.11e0_8))
    call l1%push_back (3.1 < 10.2)

    !! print Values

    call l1%begin

    do while (.not. l1%end())
        call printX (l1%getVal())

        call l1%next
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
