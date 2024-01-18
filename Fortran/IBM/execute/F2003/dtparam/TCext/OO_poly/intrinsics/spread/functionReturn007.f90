! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=base /tstdev/OO_poly/intrinsics/spread/functionReturn007.f
! opt variations: -qnol -qnodeferredlp -qreuse=none

! *********************************************************************
!*  ===================================================================
!*  DATE                       : 01/18/2005
!*  PRIMARY FUNCTIONS TESTED   : spread
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    SOURCE is the return value of a type bound procedure call.
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

        contains
        procedure, pass :: create => createBase
    end type

    type, extends(Base) :: Child    ! (20,4)
        integer(k1) j

        contains
        procedure, pass :: create => createChild
    end type

    contains

    function createBase(a)
        class(Base(*,4)), intent(in) :: a
        class(Base(:,4)), allocatable :: createBase(:)
        allocate(createBase(10), SOURCE=(/ (Base(20,4)(i), i=1,10) /))
    end function

    function createChild(a)
        class(Child(*,4)), intent(in) :: a
        class(Base(:,4)), allocatable :: createChild(:)
        allocate(createChild(20), SOURCE=(/ (Child(20,4)(i,-i), i=1,20) /))
    end function
end module

program functionReturn007
use m
    class(Base(:,4)), allocatable :: a

    allocate(Base(20,4)::a)

    select type(name1=>spread(a%create(), 2, 2))
        type is (Base(*,4))
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 1_4
    end select

    deallocate(a)
    allocate(Child(20,4)::a)

    select type(name1=>spread(a%create(), 1, 2))
        type is (Child(*,4))
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 2_4
    end select
end
