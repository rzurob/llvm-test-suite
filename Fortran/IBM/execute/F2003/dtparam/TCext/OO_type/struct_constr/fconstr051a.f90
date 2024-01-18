! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr051a.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/20/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (poly-entities used as
!                               the data-source for nonpoly rank-one array
!                               component that is of explicit-shape array)
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
    end type

    type, extends(base) :: child(k2,n1)    ! (4,1,15)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name
    end type
end module

program fconstr051a
use m
    type container(k3)    ! (4)
        integer, kind  :: k3
        type(base(k3)) :: b1(2:10)
    end type

    class (base(4)), allocatable :: b1(:)
    class (base(4)), pointer :: b2(:)

    type (child(4,1,15)), target :: c1(2:10)

    b2 => c1

    allocate (b1(2:10), source=(/(child(4,1,15)(i,'b1'), i=2,10)/))

    c1%id = (/(-i, i=2,10)/)
    c1%name = 'c1_array_of_9'

!    associate (x1 => container(4) (b1), x2 => container(4) (b2))
    call associate_replacer (container(4) (b1), container(4) (b2))

    contains

!    associate (x1 => container(4) (b1), x2 => container(4) (b2))
    subroutine associate_replacer (x1, x2)
        type(container(4)), intent(in) :: x1, x2

        if (any (x1%b1%id /= (/(j, j=2,10)/))) error stop 1_4
        if (any (x1%b1%id /= -x2%b1%id)) error stop 2_4
    end subroutine
end
