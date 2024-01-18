! GB DTP extension using:
! ftcx_dtp -qnock -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/intrinsics/cshift/functionReturn003.f
!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/cshift
! PROGRAMMER                 : Yong Du
! DATE                       : 02/02/2005
! PRIMARY FUNCTIONS TESTED   : cshift
! DRIVER STANZA              : xlf90
! DESCRIPTION                : ARRAY is the return value of a type
!                              bound procedure call.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type Base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)      i

        contains
        procedure, pass :: create => createBase
    end type

    type, extends(Base) :: Child    ! (4)
        integer(k1) j

        contains
        procedure, pass :: create => createChild
    end type

    contains

    function createBase(a)
        class(Base(4)), intent(in) :: a
        class(Base(4)), allocatable :: createBase(:)
        allocate(createBase(10), SOURCE=(/ (Base(4)(i), i=1,10) /))
    end function

    function createChild(a)
        class(Child(4)), intent(in) :: a
        class(Base(4)), allocatable :: createChild(:)
        allocate(createChild(20), SOURCE=(/ (Child(4)(i,-i), i=1,20) /))
    end function
end module

program functionReturn003
use m
    class(Base(4)), allocatable :: a

    allocate(Base(4)::a)

    select type(name1=>cshift(a%create(), 5, 1))
        type is (Base(4))
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 1_4
    end select

    deallocate(a)
    allocate(Child(4)::a)

    select type(name1=>cshift(a%create(), -12))
        type is (Child(4))
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 2_4
    end select
end
