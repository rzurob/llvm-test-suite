! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=base /tstdev/OO_poly/class/fclass008.f
! opt variations: -qck -qnol -qnodeferredlp -qreuse=none

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/25/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : CLASS keyword (reshape, array constructor with
!                               poly-entities and associate)
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

        procedure :: print => printBase
    end type

    type, extends(base) :: child    ! (20,4)
        character(n1) :: name

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printBase (b)
        class (base(*,4)), intent(in) :: b

        print *, b%id
    end subroutine


    subroutine printChild (b)
        class (child(*,4)), intent(in) :: b

        print *, b%id, b%name
    end subroutine
end module

program fclass008
use m
    class (base(:,4)), allocatable :: b1, b2, b3, b4

    allocate (b1, source=child(20,4)(1, 'b1'))
    allocate (b2, source=child(20,4)(2, 'b2'))
    allocate (b3, source=child(20,4)(3, 'b3'))
    allocate (b4, source=child(20,4)(4, 'b4'))


    associate (x => reshape ((/b1, b2, b3, b4/), (/2,2/)))
        if (any(shape (x) /= (/2,2/))) error stop 1_4

        call x(1,1)%print
        call x(2,1)%print
        call x(1,2)%print
        call x(2,2)%print
    end associate
end
