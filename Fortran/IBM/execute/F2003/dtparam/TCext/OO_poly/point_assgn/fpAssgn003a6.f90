! GB DTP extension using:
! ftcx_dtp -qck -ql -qdeferredlp -qreuse=base /tstdev/OO_poly/point_assgn/fpAssgn003a6.f
! opt variations: -qnock -qnol -qnodeferredlp -qreuse=none

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/19/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (non-poly data pointer
!*                               assigned to poly, or ancestor component of the
!*                               data-target; test its dynamic types using
!*                               type bound in procedure calls)
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
        integer(k1)   :: id = 0

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child(k2)    ! (20,4,1)
        integer, kind             :: k2
        character(kind=k2,len=n1) :: name = ''

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printBase (b)
        class (base(*,4)), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child(*,4,1)), intent(in) :: b

        print *, b%id, b%name
    end subroutine

    subroutine printData (b)
        class (base(*,4)), intent(in) :: b

        call b%print
    end subroutine
end module

program fpAssgn003a6
use m, only : base, child, printData

    type (base(:,4)), pointer :: b

    class (base(:,4)), pointer :: b_ptr
    class (child(20,4,1)), pointer :: c1

    allocate (c1)

    c1%id = 1
    c1%name = 'c1'

    b => c1%base

    call printData (b)
    call printData (c1)

    b_ptr => c1

    call printData (b_ptr)

    b => b_ptr

    call printData (b)
    deallocate (c1)
end
