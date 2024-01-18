! GB DTP extension using:
! ftcx_dtp -qck -ql -qdeferredlp -qreuse=base /tstdev/OO_poly/point_assgn/fpAssgn005.f
! opt variations: -qnock -qnol -qnodeferredlp -qreuse=none

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/05/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : pointer assignment (polymorphic pointer
!*                               assigned to compatible target, pass binding
!*                               used for verification)
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

        procedure, pass :: print => printBase
    end type

    type, extends(base) :: child(k2)    ! (20,4,1)
        integer, kind             :: k2
        character(kind=k2,len=n1) :: name

        contains

        procedure, pass :: print => printChild
    end type

    contains

    subroutine printBase (b)
        class (base(*,4)), intent(in) :: b

        print *, 'type base with id = ', b%id
    end subroutine

    subroutine printChild (b)
        class (child(*,4,1)), intent(in) :: b

        print *, 'type Child with id = ', b%id, '; name = ', b%name
    end subroutine
end module

program fpAssgn005
use m

    type (base(20,4)), target :: b1
    type (child(20,4,1)), target :: c1

    class (base(:,4)), pointer :: b_ptr
    class (child(:,4,1)), pointer :: c_ptr

    b1 = base(20,4) (1)
    c1 = child(20,4,1) (2, 'c1')

    !! target is b1
    b_ptr => b1

    call b_ptr%print

    !! target is c1
    b_ptr => c1

    call b_ptr%print

    !! target is c1%base
    b_ptr => c1%base

    call b_ptr%print

    !! target is c1
    c_ptr => c1

    call c_ptr%print

    !! print base data
    call c_ptr%base%print
end
