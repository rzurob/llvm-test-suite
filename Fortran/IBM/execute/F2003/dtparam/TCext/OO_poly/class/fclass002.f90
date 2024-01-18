! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/class/fclass002.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/30/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : class (argument association,
!*                               poly-dummy-arg-array; resolve type-bounds)
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
    type base(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        contains

        procedure, nopass :: print => printBase
    end type

    type, extends(base) :: child    ! (4,20)
        integer(k1) :: id

        contains

        procedure, nopass :: print => printChild
    end type

    contains

    subroutine printBase
        print *, 'base'
    end subroutine

    subroutine printChild
        print *, 'child'
    end subroutine
end module

program fclass002
use m

    type (base(4,20)) :: b1(3)
    type (child(4,20)) :: c1(30)


    call abc (b1)

    call abc (c1)

    contains

    subroutine abc (a)
        class (base(4,*)), intent(in) :: a(:)

        print *, size(a)
        call a%print
    end subroutine
end
