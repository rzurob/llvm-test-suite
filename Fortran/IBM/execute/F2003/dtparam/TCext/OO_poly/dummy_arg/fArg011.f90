! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg011.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/23/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (dummy-arg as selector in
!                               ASSOCIATE construct)
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

    type, extends (base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name

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

    subroutine printBaseInAssociate (b)
        class (base(4)), intent(in) :: b

        associate (x => b)
            call x%print
        end associate
    end subroutine
end module

program fArg011
use m
    type (base(4)) :: b1 = base(4) (1)

    type (child(4,1,20)) :: c1 = child(4,1,20) (2, 'c1')

    class (base(4)), pointer :: b_ptr

    allocate (b_ptr, source=child(4,1,20)(3,'b_ptr'))

    call printBaseInAssociate (b1)

    call printBaseInAssociate (c1)

    call printBaseInAssociate (b_ptr)
end
