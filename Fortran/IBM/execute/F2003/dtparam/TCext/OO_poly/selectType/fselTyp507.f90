! GB DTP extension using:
! ftcx_dtp -qck -ql -qdeferredlp -qreuse=base /tstdev/OO_poly/selectType/fselTyp507.f
! opt variations: -qnock -qnol -qnodeferredlp -qreuse=none

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/1/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : select type (deferred binding call in select
!                               type construct where selector is an abstract
!                               type by declaration)
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
    type, abstract :: base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)      id

        contains

        procedure (print1), pass (b), deferred :: print
    end type

    type, extends (base) :: child(k2)    ! (20,4,1)
        integer, kind             :: k2
        character(kind=k2,len=n1) :: name

        contains

        procedure :: print => printChild
    end type

    interface
        subroutine print1(b)
        import base
            class (base(*,4)), intent(in) :: b
        end subroutine
    end interface

    contains

    subroutine printChild (b)
        class (child(*,4,1)), intent(in) :: b

        print *, b%id, b%name
    end subroutine
end module

program fselTyp507
use m
    class (base(:,4)), pointer :: b1

    allocate (b1, source=child(20,4,1)(10, 'b1'))

    select type (b1)
        class is (base(*,4))
            call b1%print
        class default
            print *, 'wrorng'
    end select
end
