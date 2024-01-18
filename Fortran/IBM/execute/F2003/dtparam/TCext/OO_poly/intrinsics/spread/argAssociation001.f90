! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=base /tstdev/OO_poly/intrinsics/spread/argAssociation001.f
! opt variations: -qnol -qnodeferredlp -qreuse=none

! *********************************************************************
!*  ===================================================================
!*  DATE                       : 01/20/2005
!*  PRIMARY FUNCTIONS TESTED   : spread
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    SOURCE is a dummy argument. Dummy argument is non-pointer,
!*  non-allocatable, non-poly, and is scalar.
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  ===================================================================
!*  REVISION HISTORY
!*                    MM/DD/YY :
!*                        Init :
!*                    Comments :
!*  ===================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901

module m
    type Base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)      i
    end type

    type, extends(Base) :: Child    ! (20,4)
        integer(k1) j
    end type
end module

program argAssociation001
use m
    type(Base(20,4)) :: b
    class(Base(:,4)), pointer :: c

    b = Base(20,4)(10)
    allocate(c, SOURCE=Child(20,4)(3,4))

    call sub1(b, c)

    contains

    subroutine sub1(arg1, arg2)
        type(Base(*,4)) :: arg1
        type(Base(*,4)) :: arg2

        print *, spread(arg1, 1, 3)
        print *, size(spread(arg1, 1, 3))
        print *, shape(spread(arg1, 1, 3))

        print *, spread(arg2, 1, 5)
        print *, size(spread(arg2, 1, 5))
        print *, shape(spread(arg2, 1, 5))
    end subroutine
end
