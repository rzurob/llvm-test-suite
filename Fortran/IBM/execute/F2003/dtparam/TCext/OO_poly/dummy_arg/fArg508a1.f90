! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg508a1.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/20/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (INTENT(OUT) dummy-arg's
!                               default initialization; poly-actual-arg
!                               associated with non-poly-dummy-arg; use scalars)
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

    class (base(4)), allocatable :: b1_m(:)

    contains

    logical function isDefault (b)
        type (base(4)), intent(out) :: b

        isDefault = (b%id == -1)
    end function

    subroutine printBase (b)
        class (base(4)), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child(4,1,*)), intent(in) :: b

        print *, b%id, b%name
    end subroutine
end module

program fArg508a1
use m
    type (child(4,1,20)) :: c1 (3)

    c1 = (/child(4,1,20) (1,'c1_1'), child(4,1,20)(2,'c1_2'), child(4,1,20)(3,'c1_3')/)

    allocate (b1_m(3:5), source=c1)

    if (.not. isDefault(b1_m(3))) error stop 1_4

    call b1_m(3)%print
    call b1_m(4)%print
    call b1_m(5)%print
end
