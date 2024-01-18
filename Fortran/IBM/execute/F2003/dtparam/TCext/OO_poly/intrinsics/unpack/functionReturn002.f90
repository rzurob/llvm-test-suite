! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=base /tstdev/OO_poly/intrinsics/unpack/functionReturn002.f
! opt variations: -qnol -qnodeferredlp -qreuse=none

!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/unpack
! PROGRAMMER                 : Yong Du
! DATE                       : 02/25/2005
! PRIMARY FUNCTIONS TESTED   : unpack
! DRIVER STANZA              : xlf90
! DESCRIPTION                : VECTOR or FIELD is the return value of a
!                              type bound procedure call.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type Base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)      i

        contains
        procedure, pass :: createOne => createBaseOne
        procedure, pass :: createTwo => createBaseTwo
    end type

    type, extends(Base) :: Child    ! (20,4)
        integer(k1) j

        contains
        procedure, pass :: createOne => createChildOne
        procedure, pass :: createTwo => createChildTwo
    end type

    contains

    function createBaseOne(a)
        class(Base(*,4)), intent(in) :: a
        class(Base(:,4)), allocatable :: createBaseOne(:)
        allocate(createBaseOne(10),SOURCE=(/(Base(20,4)(-i),i=1,10)/))
    end function

    function createBaseTwo(a)
        class(Base(*,4)), intent(in) :: a
        class(Base(:,4)), allocatable :: createBaseTwo(:,:)
        allocate(createBaseTwo(3,4), &
         SOURCE=reshape((/(Base(20,4)(i),i=1,12)/), (/3,4/)))
    end function

    function createChildOne(a)
        class(Child(*,4)), intent(in) :: a
        class(Base(:,4)), allocatable :: createChildOne(:)
        allocate(createChildOne(9),SOURCE=(/(Child(20,4)(-i,i), i=11,19)/))
    end function

    function createChildTwo(a)
        class(Child(*,4)), intent(in) :: a
        class(Base(:,4)), allocatable :: createChildTwo(:,:)
        allocate(createChildTwo(4,5),SOURCE=reshape((/(Child(20,4)(i,-i), &
         i=1,20)/),(/4,5/)))
    end function
end module

program functionReturn002
use m
    class(Base(:,4)), allocatable :: a
    logical :: m1(20)

    allocate(Base(20,4)::a)
    m1 = (/.FALSE.,.TRUE.,.TRUE.,.FALSE.,.TRUE.,.TRUE.,.FALSE., &
     .FALSE.,.TRUE.,.FALSE.,.TRUE.,.FALSE.,.FALSE.,.FALSE.,.TRUE., &
     .FALSE.,.FALSE.,.TRUE.,.TRUE.,.FALSE./)

    select type(name1=>unpack(a%createOne(), reshape(m1,(/3,4/)), &
     a%createTwo()))
        type is (Base(*,4))
            print *, name1
            print *, shape(name1)
        class default
            error stop 1_4
    end select

    deallocate(a)
    allocate(Child(20,4)::a)

    select type(name1=>unpack(a%createOne(), reshape(m1,(/4,5/)), &
     a%createTwo()))
        type is (Child(*,4))
            print *, name1
            print *, shape(name1)
        class default
            error stop 2_4
    end select
end
