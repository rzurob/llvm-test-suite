! GB DTP extension using:
! ftcx_dtp -qnol -qnodefaultpv /tstdev/OO_poly/point_assgn/fpAssgn011a1.f
! opt variations: -qck -ql -qdefaultpv

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/07/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (self assignment for
!                               the poly-pointer array)
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
    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)   :: id

        contains

        procedure, pass :: print => printBase
    end type

    type, extends(base) :: child(n1)    ! (4,20)
        integer, len  :: n1
        character(n1) :: name
        contains

        procedure, pass :: print => printChild
    end type

    contains

    subroutine printBase (b)
        class (base(4)), intent(in) :: b

        print *, 'id = ', b%id
    end subroutine

    subroutine printChild (b)
        class (child(4,*)), intent(in) :: b

        print *, 'id = ', b%id, '; name = ', b%name
    end subroutine
end module

program fpAssgn011a1
use m
    type container(k2)    ! (4)
        integer, kind            :: k2
        class(base(k2)), pointer :: data (:) => null()
    end type

    type (container(4)) :: co1

    type (child(4,20)), target :: c1(10)

    class (base(4)), pointer :: x(:)

    c1 = (/(child(4,20)(i, 'c1'), i=1, 10)/)

    x => c1

    x => x(::4)

    if (size(x) /= 3) error stop 1_4

    call x(1)%print
    call x(2)%print
    call x(3)%print

    co1 = container(4) (c1)

    co1 = container(4) (co1%data(::3))

    if (size(co1%data) /= 4) error stop 2_4

    call co1%data(1)%print
    call co1%data(2)%print
    call co1%data(3)%print
    call co1%data(4)%print
end
