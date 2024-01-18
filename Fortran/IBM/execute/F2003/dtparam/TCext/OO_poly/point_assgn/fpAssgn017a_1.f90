! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qnodeferredlp -qreuse=base /tstdev/OO_poly/point_assgn/fpAssgn017a_1.f
! opt variations: -qck -qnok -qnol -qdefaultpv -qdeferredlp -qreuse=self -qreuse=none

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (test linked-list)
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)   :: id

        contains

        procedure :: print => printBase
        final :: finalizeBase
    end type

    type, extends(base) :: child    ! (20,4)
        character(n1) :: name

        contains

        procedure :: print => printChild
        final :: finalizeChild
    end type

    type (base(20,4)) :: b1 = base(20,4) (10)
    type (child(20,4)) :: c1 = child(20,4) (20, 'c1')

    type (base(20,4)) :: b2 (2)
    type (child(20,4)) :: c2 (5)

    contains

    subroutine printBase (b)
        class (base(*,4)), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child(*,4)), intent(in) :: b

        print *, b%id, b%name
    end subroutine

    subroutine finalizeBase (b)
        type (base(*,4)), intent(in) :: b

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeChild (c)
        type (child(*,4)), intent(in) :: c

        print *, 'finalizeChild'
    end subroutine
end module


module m1
use m
    type node(k2,n2)    ! (4,20)
        integer, kind               :: k2
        integer, len                :: n2
        type(node(k2,n2)), pointer  :: next => null()
        class(base(n2,k2)), pointer :: data => null()
    end type

    type llist(k3,n3)    ! (4,20)
        integer, kind              :: k3
        integer, len               :: n3
        type(node(k3,n3)), pointer :: head => null()

        contains

        procedure :: print => printLlist
        procedure :: push_back => addNode2End
        final :: finalizeList
    end type

    contains

    subroutine printLlist (l)
        class (llist(4,*)), intent(in) :: l

        type (node(4,20)), pointer :: iterator

        iterator => l%head

        do while (associated (iterator))
            if (associated (iterator%data)) then
                call iterator%data%print
            else
                print *, 'empty node'
            end if

            iterator => iterator%next
        end do
    end subroutine

    subroutine finalizeList (l)
        type (llist(4,*)), intent(inout) :: l

        type (node(4,20)), pointer :: tail, iterator
        integer(4) :: i = 0

        tail => l%head

        !! find the tail and deallocate the node along the way
        do while (associated (tail))
            iterator => tail

            tail => tail%next

            if (associated (iterator%data)) deallocate (iterator%data)
            deallocate (iterator)

            i = i + 1
        end do

        print *, i, 'nodes deallocated'
    end subroutine

    subroutine addNode2End (l, n)
        class (llist(4,*)), intent(inout) :: l
        class (base(*,4)), intent(in) :: n

        type (node(4,20)), pointer :: tail

        !! treat the first node special

        if (.not. associated (l%head)) then
            allocate (l%head)

            allocate (l%head%data, source=n)
            return
        end if

        tail => l%head

        do while (associated (tail%next))
            tail => tail%next
        end do

        allocate (tail%next)
        allocate (tail%next%data, source=n)
    end subroutine
end module


program fpAssgn017a
use m
    b2%id = (/-1, -2/)

    c2%id = (/(i*100, i=1,5)/)
    c2%name = (/('c2_'//char(ichar('0')+i), i=1,5)/)

    call testLlist
end

subroutine testLlist
use m1
    type (llist(4,20)) l1

    call l1%push_back (c1)

    call l1%push_back (b1)

    call l1%push_back (b2(2))

    call l1%push_back (b2(1))

    call l1%push_back (c2(3))

    call l1%push_back (c2(5))

    call l1%push_back (c2(2)%base)

    call l1%print
end subroutine
