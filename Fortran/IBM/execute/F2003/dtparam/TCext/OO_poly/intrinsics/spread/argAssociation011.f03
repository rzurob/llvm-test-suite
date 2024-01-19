! GB DTP extension using:
! ftcx_dtp -ql -qreuse=base /tstdev/OO_poly/intrinsics/spread/argAssociation011.f
! opt variations: -qnol -qreuse=none

! *********************************************************************
!*  ===================================================================
!*  DATE                       : 01/20/2005
!*  PRIMARY FUNCTIONS TESTED   : spread
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    SOURCE is a dummy argument. Dummy argument is a pointer or
!*  allocatable, unlimited poly, and is scalar.
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

program argAssociation011
use m
    class(*), pointer :: b
    class(*), allocatable :: c

    allocate(b, SOURCE=Child(20,4)(j=11,i=10))
    allocate(c, SOURCE=Base(20,4)(6))

    call sub1(b, c)

    contains

    subroutine sub1(arg1, arg2)
        class(*), pointer :: arg1
        class(*), allocatable :: arg2

        select type(name1=>spread(arg1, 1, 5))
            type is (Child(*,4))
                print *, name1
                print *, size(name1)
                print *, shape(name1)
            class default
                error stop 1_4
        end select

        select type(name1=>spread(arg2, 1, 10))
            type is (Base(*,4))
                print *, name1
                print *, size(name1)
                print *, shape(name1)
            class default
                error stop 2_4
        end select
    end subroutine
end
