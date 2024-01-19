! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_poly/point_assgn/fpAssgn029.f
! opt variations: -qck -ql

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/07/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (poly-pointer array
!*                               assigned to an array section of parent
!*                               components of extended target array)
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

        procedure :: print => printBase
    end type

    type, extends(base) :: child(n1)    ! (4,20)
        integer, len  :: n1
        character(n1) :: name

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printBase (b)
        class (base(4)), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child(4,*)), intent(in) :: b

        print *, b%id, b%name
    end subroutine
end module

program fpAssgn029
use m
    class (base(4)), pointer :: b_ptr (:)

    type(child(4,20)), target :: c1 (2:10)

    c1 = (/(child(4,20)(i, 'c1'), i=2,10)/)

    b_ptr => c1(3::2)%base

    do i = 1, 4
        call b_ptr(i)%print
    end do

end
