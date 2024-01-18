! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp -qreuse=none /tstdev/OO_poly/point_assgn/fpAssgn030a2.f
! opt variations: -qck -qnok -qnol -qnodeferredlp -qreuse=base

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/12/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (poly data used as
!                                selector; pointer assignment occurrs in
!                                ASSOCIATE block; also use array section)
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

    type, extends (base) :: child(k2,n2)    ! (20,4,4,20)
        integer, kind :: k2
        integer, len  :: n2
        character(n2) :: name

        contains

        procedure :: print => printChild
    end type

    class (base(:,4)), pointer :: b1_m

    contains

    subroutine printBase (b)
        class (base(*,4)), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child(*,4,4,*)), intent(in) :: b

        print *, b%id, b%name
    end subroutine
end module

program fpAssgn030a2
use m
    class (base(:,4)), allocatable, target :: b1 (:)

    allocate (b1 (2:11), source=child(20,4,4,20)(0, 'b1'))

    associate (x => b1(::2))
        do i = 1, size (x)
            b1_m => x(i)

            b1_m%id = i*2

            call b1_m%print
        end do
    end associate

    do i = 2, 11
        call b1(i)%print
    end do
end
