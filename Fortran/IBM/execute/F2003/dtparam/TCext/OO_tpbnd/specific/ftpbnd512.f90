! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qdeferredlp /tstdev/OO_tpbnd/specific/ftpbnd512.f
! opt variations: -qnok -qnol -qdefaultpv -qnodeferredlp -qreuse=self

! SCCS ID Information
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
    type, private :: node(k1,n1)    ! (4,20)
        integer, kind             :: k1
        integer, len              :: n1
        class(*), allocatable :: data

        type(node(k1,:)), pointer :: next => null()
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
    end type


    contains

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

end module

program ftpbnd512
use m
    type (list(4,20)) :: l1

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
