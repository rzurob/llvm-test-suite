! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_poly/allocate/falloc003a2_2.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/09/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (array constructor as the source-expr)
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
        integer(k1)   :: id = -1

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name = 'default'

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printBase (b)
        class (base(4)), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child(4,1,*)), intent(in) :: b

        print *, b%id, b%name
    end subroutine
end module


program falloc003a2_1
use m
    type (base(4)), allocatable :: b1(:)
    type (child(4,1,20)), pointer :: c_ptr(:)

    type (child(4,1,20)) :: c1 = child(4,1,20) (10, 'c1')
    type (child(4,1,20)) :: c2

    allocate (b1(2), source=(/c1%base, base(4)(20)/))

    call b1(1)%print
    call b1(2)%print

    allocate (c_ptr (2), source= (/c2, c1/))

    call c_ptr(1)%print
    call c_ptr(2)%print

    deallocate (c_ptr, b1)
end
