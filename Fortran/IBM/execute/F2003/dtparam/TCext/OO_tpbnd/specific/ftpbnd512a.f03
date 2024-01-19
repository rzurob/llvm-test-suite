! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qdeferredlp -qreuse=none /tstdev/OO_tpbnd/specific/ftpbnd512a.f
! opt variations: -qnok -qnol -qdefaultpv -qnodeferredlp -qreuse=self

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/01/2005
!*
!*  DESCRIPTION                : specific type bound (linked-list binding; copy
!                               operation)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type, private :: node(k1,n1)    ! (4,20)
        integer, kind             :: k1
        integer, len              :: n1
        class(*), allocatable :: data

        type(node(k1,:)), pointer :: next => null()

        contains

        final :: finalizeNode
    end type

    type list(k2,n2)    ! (4,20)
        integer, kind                      :: k2
        integer, len                       :: n2
        type(node(k2,:)), pointer, private :: head => null()
        type(node(k2,:)), pointer, private :: current => null()
        type(node(k2,:)), pointer, private :: tail => null()

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
        type (node(4,*)), intent(inout) :: n
        if (associated (n%next)) deallocate (n%next)
    end subroutine

    subroutine finalizeList (l)
        type (list(4,*)), intent(inout) :: l

        nullify (l%current, l%tail)

        if (associated (l%head)) deallocate (l%head)
    end subroutine


    class (*) function getCurrentNodeVal (l)
        allocatable getCurrentNodeVal
        class (list(4,*)), intent(in) :: l

        if (associated (l%current)) then
            allocate (getCurrentNodeVal, source=l%current%data)
        else
            print *, 'no data selected'
            error stop 10_4
        end if
    end function

    subroutine pushDataToEnd (l, x)
        class (list(4,*)), intent(inout) :: l
        class (*), intent(in) :: x

        if (.not. associated (l%head)) then !! very first node(4,20)
            allocate (node(4,20) :: l%head)
            allocate (l%head%data, source=x)

            l%tail => l%head
        else
            allocate (l%tail%next, source=node(4,20)(data=x))

            l%tail => l%tail%next
        end if
    end subroutine

    subroutine startIterator (l)
        class (list(4,*)), intent(inout) :: l

        l%current => l%head
    end subroutine

    logical function endIterator (l)
        class (list(4,*)), intent(in) :: l

        endIterator = .not. associated (l%current)
    end function

    subroutine move2NextNode (l)
        class (list(4,*)), intent(inout) :: l

        if (associated (l%current)) l%current => l%current%next
    end subroutine


    subroutine copyL2toL1 (l1, l2)
        class (list(4,*)), intent(out) :: l1
        class (list(4,*)), intent(in) :: l2

        type (node(4,:)), pointer :: iterator

        !! set up the head in l1
        if (associated (l2%head)) then
            allocate (l1%head, source=node(4,20)(l2%head%data))

            l1%tail => l1%head
            iterator => l2%head%next
        end if

        !! allocate the rest nodes
        do while (associated (iterator))
            allocate (l1%tail%next, source=node(4,20)(iterator%data))

            l1%tail => l1%tail%next
            iterator => iterator%next
        end do
    end subroutine

    subroutine clearList (l)
        class (list(4,*)), intent(inout) :: l

        call finalizeList (l)
    end subroutine
end module

program ftpbnd512a
use m
    type (list(4,20)) :: l1, l2

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
