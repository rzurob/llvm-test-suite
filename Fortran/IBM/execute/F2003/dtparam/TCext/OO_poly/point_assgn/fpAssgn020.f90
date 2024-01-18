! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qnodeferredlp -qreuse=base /tstdev/OO_poly/point_assgn/fpAssgn020.f
! opt variations: -qck -qnok -qnol -qdefaultpv -qdeferredlp -qreuse=self -qreuse=none

! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fpAssgn020.f
! %VERIFY: fpAssgn020.out:fpAssgn020.vf
! %STDIN:
! %STDOUT: fpAssgn020.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assgnment (a realistic example of
!*                               linked-list)
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
    type dataType(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        contains

        procedure :: print => printDataType
    end type

    type, extends(dataType) :: moduleData    ! (4,20)
        integer(k1) :: id = 0

        contains

        procedure :: print => printmoduleData
    end type

    contains

    subroutine printDataType (d)
        class (dataType(4,*)), intent(in) :: d

        print *, 'empty type; do NOT reference'
    end subroutine

    subroutine printmoduleData (d)
        class (moduleData(4,*)), intent(in) :: d

        print *, 'moduleData:', d%id
    end subroutine
end module

module m1
use m, only : dataType, moduleData

    type, private :: node(k2,n2)    ! (4,20)
        integer, kind                   :: k2
        integer, len                    :: n2
        type(node(k2,n2)), pointer      :: next => null()
        class(dataType(k2,n2)), pointer :: data => null()
    end type

    type container(k3,n3)    ! (4,20)
        integer, kind              :: k3
        integer, len               :: n3
        type(node(k3,n3)), pointer :: head => null()

        contains

        procedure :: print => printContainer
        procedure :: size => countNodes
        procedure :: push_back => addNode
        procedure :: clear => deleteAllNodes
    end type

    contains

    subroutine printContainer (co)
        class (container(4,*)), intent(in), target :: co

        type (node(4,20)), pointer :: iterator

        iterator => co%head

        do while (associated (iterator))
            if (associated (iterator%data)) call iterator%data%print

            iterator => iterator%next
        end do
    end subroutine

    integer*4 function countNodes (co)
        class (container(4,*)), intent(in), target :: co

        type (node(4,20)), pointer :: iterator

        countNodes = 0

        iterator => co%head

        do while (associated (iterator))
            countNodes = countNodes + 1
            iterator => iterator%next
        end do
    end function

    subroutine addNode (co, d1)
        class (container(4,*)), intent(inout) :: co
        class (dataType(4,*)), target :: d1

        type (node(4,20)), pointer :: tail

        if (.not. associated (co%head)) then
            allocate (co%head)
            co%head = node(4,20) (data = d1)
        else
            tail => co%head

            do while (associated (tail) .and. (associated (tail%next)))
                tail => tail%next
            end do

            allocate (tail%next)

            tail%next = node(4,20) (data = d1)
        end if
    end subroutine

    subroutine deleteAllNodes (co)
        class (container(4,*)), intent(inout) :: co

        type (node(4,20)), pointer :: iterator, temp

        iterator => co%head

        do while (associated (iterator) .and. associated (iterator%next))
            temp => iterator%next

            deallocate (iterator)

            iterator => temp
        end do

        deallocate (iterator)
        nullify (co%head)
    end subroutine
end module

module m2
use m, only : dataType

    type, extends (dataType) :: mData(k4)    ! (4,20,2)
        integer, kind :: k4
        logical(k4)      flag

        contains

        procedure :: print => printMdata
    end type

    type, extends (mData) :: nData    ! (4,20,2)
        character(n1) :: name

        contains

        procedure :: print => printNdata
    end type

    contains

    subroutine printMdata (d)
        class (mData(4,*,2)), intent(in) :: d

        print *, 'mData:', d%flag
    end subroutine

    subroutine printNdata (d)
        class (nData(4,*,2)), intent(in) :: d

        print *, 'nData:', d%flag, d%name
    end subroutine
end module

program fpAssgn020
use m1
use m2

    type (container(4,20)) list

    type (moduleData(4,20)), target, save :: md1, md2
    type (mData(4,20,2)), save, target :: md3
    type (nData(4,20,2)), save, target :: md4

    type (nData(4,20,2)), allocatable, target :: nd1

    class (dataType(4,20)), pointer :: md5
    class (mData(4,20,2)), pointer :: nd2

    type (mData(4,20,2)), pointer :: md6

    md1 = moduleData(4,20) (10)
    md2 = moduleData(4,20) (20)

    md3 = mData(4,20,2) (1==1)
    md4 = nData(4,20,2) (1<3, 'md4')

    allocate (nd1)
    nd1 = nData(4,20,2) (.true., 'nd1')

    md5 => nd1

    !! let's try a non-poly pointer to a poly target
    nd2 => nd1
    md6 => nd2

    call list%push_back (md1)
    call list%push_back (md2)
    call list%push_back (md3)
    call list%push_back (md4)
    call list%push_back (md5)
    call list%push_back (md6)

    call list%print

    if (list%size() /= 6) error stop 1_4

    call list%clear

    if (list%size() /= 0) error stop 2_4
end
