! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=base /tstdev/OO_poly/intrinsics/spread/argAssociation007.f
! opt variations: -qnol -qnodeferredlp -qreuse=none

! *********************************************************************
!*  ===================================================================
!*  DATE                       : 01/20/2005
!*  PRIMARY FUNCTIONS TESTED   : spread
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    SOURCE is a dummy argument. Dummy argument is a pointer or
!*  allocatable, non-poly, and is scalar.
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

program argAssociation007
use m
    type(Base(:,4)), pointer :: b
    type(Child(:,4)), allocatable :: c

    allocate(b, SOURCE=Base(20,4)(i=10))
    allocate(c, SOURCE=Child(20,4)(j=6, i=9))

    call sub1(b, c)

    contains

    subroutine sub1(arg1, arg2)
        type(Base(:,4)), pointer :: arg1
        type(Child(:,4)), allocatable :: arg2

        associate(name1=>spread(arg1, 1, 6))
            if(.NOT. same_type_as(name1, Base(20,4)(1))) error stop 1_4
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        end associate

        associate(name1=>spread(arg2, 1, 4))
            if(.NOT. same_type_as(name1, Child(20,4)(1,1))) error stop 2_4
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        end associate
    end subroutine
end
