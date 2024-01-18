! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qnodefaultpv -qdeferredlp -qreuse=self -qreuse=base /tstdev/OO_poly/point_assgn/fpAssgn017a.f
! opt variations: -qnock -qnok -qnol -qdefaultpv -qnodeferredlp -qreuse=none

!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fpAssgn017a.f
! %VERIFY: fpAssgn017a.out:fpAssgn017a.vf
! %STDIN:
! %STDOUT: fpAssgn017a.out
! %EXECARGS:
! %POSTCMD: 
! %END
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 07/08/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : data pointer assignment (a test of the
!                               linked-list that uses pointer assignment in
!                               putting data into the list)
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
    end type

    type, extends(base) :: child(k2)    ! (20,4,1)
        integer, kind             :: k2
        character(kind=k2,len=n1) :: name

        contains

        procedure :: print => printChild
    end type

    type (base(20,4)), target :: b1 = base(20,4) (10)
    type (child(20,4,1)), target :: c1 = child(20,4,1) (20, 'c1')

    type (base(20,4)), target :: b2 (2)
    type (child(20,4,1)), target :: c2 (5)

    contains

    subroutine printBase (b)
        class (base(*,4)), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child(*,4,1)), intent(in) :: b

        print *, b%id, b%name
    end subroutine
end module


module m1
use m
    type node(k3,n2)    ! (4,20)
        integer, kind              :: k3
        integer, len               :: n2
        type(node(k3,:)), pointer  :: next => null()
        class(base(:,k3)), pointer :: data => null()
    end type

    type llist(k4,n3)    ! (4,20)
        integer, kind             :: k4
        integer, len              :: n3
        type(node(k4,:)), pointer :: head => null()

        contains

        procedure :: print => printLlist
        procedure :: push_back => addNode2End
        final :: finalizeList
    end type

    contains

    subroutine printLlist (l)
        class (llist(4,*)), intent(in) :: l

        type (node(4,:)), pointer :: iterator

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

        type (node(4,:)), pointer :: tail, iterator
        integer(4) :: i = 0

        tail => l%head

        !! find the tail and deallocate the node along the way
        do while (associated (tail))
            iterator => tail

            tail => tail%next

            deallocate (iterator)

            i = i + 1
        end do

        print *, i, 'nodes deallocated'
    end subroutine

    subroutine addNode2End (l, n)
        class (llist(4,*)), intent(inout) :: l
        class (base(*,4)), target, intent(in) :: n

        type (node(4,:)), pointer :: tail

        !! treat the first node special

        if (.not. associated (l%head)) then
            allocate (l%head, source=node(4,20) (data=n))
            return
        end if

        tail => l%head

        do while (associated (tail%next))
            tail => tail%next
        end do

        allocate (tail%next, source=node(4,20) (data=n))
    end subroutine
end module


program fpAssgn017a
use m
    b2 = (/base(20,4) (-1), base(20,4)(-2)/)

    c2 = (/(child(20,4,1) (i*100, name='c2_'//char(ichar('0')+i)), i=1,5)/)

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

    call l1%print
end subroutine
