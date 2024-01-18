! GB DTP extension using:
! ftcx_dtp -qnock -qnok -ql -qreuse=self -qdefaultpv -qdeferredlp /tstdev/OO_poly/intrinsics/null/functionReturn002.f
!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/null
! PROGRAMMER                 : Yong Du
! DATE                       : 03/02/2005
! PRIMARY FUNCTIONS TESTED   : null
! DRIVER STANZA              : xlf90
! DESCRIPTION                : MOLD is the return value of a type bound
!                              procedure call.
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

    type, extends(Base) :: Child(k2)    ! (4,4)
        integer, kind :: k2
        integer(k2)      j

        contains
        procedure, pass :: create => createChild
    end type

    contains

    function createBase(a)
        class(Base(4)), intent(in) :: a
        class(Base(4)), allocatable :: createBase(:,:)
        allocate(createBase(3,4),SOURCE=reshape((/(Base(4)(i),i=1,12)/), &
         (/3,4/)))
    end function

    function createChild(a)
        class(Child(4,4)), intent(in) :: a
        class(Base(4)), allocatable :: createChild(:,:)
        allocate(createChild(4,5),SOURCE=reshape((/(Child(4,4)(i,-i), &
         i=1,20)/),(/4,5/)))
    end function
end module

program functionReturn002
use m
    class(Base(4)), allocatable :: a

    allocate(Base(4)::a)

    if(allocated(null(a%create()))) error stop 1_4
    if(.NOT. same_type_as(a%create(), Base(4)(1))) error stop 2_4
    if(.NOT. same_type_as(null(a%create()), Base(4)(1))) error stop 3_4

    deallocate(a)
    allocate(Child(4,4)::a)

    if(allocated(null(a%create()))) error stop 4_4
    if(.NOT. same_type_as(a%create(), Child(4,4)(1,1))) error stop 5_4
    if(.NOT. same_type_as(null(a%create()), Base(4)(1))) error stop 6_4
end
