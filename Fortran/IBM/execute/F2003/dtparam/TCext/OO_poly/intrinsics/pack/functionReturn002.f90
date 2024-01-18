! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/OO_poly/intrinsics/pack/functionReturn002.f
! opt variations: -qnol -qnodeferredlp -qreuse=base

!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/pack
! PROGRAMMER                 : Yong Du
! DATE                       : 02/21/2005
! PRIMARY FUNCTIONS TESTED   : pack
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
    type Base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)      i

        contains
        procedure, pass :: create => createBase
    end type

    type, extends(Base) :: Child(n2,k2)    ! (20,4,20,4)
        integer, kind :: k2
        integer, len  :: n2
        integer(k2)      j

        contains
        procedure, pass :: create => createChild
    end type

    contains

    function createBase(a)
        class(Base(*,4)), intent(in) :: a
        class(Base(:,4)), allocatable :: createBase(:,:)
        allocate(createBase(3,4),SOURCE=reshape((/(Base(20,4)(i),i=1,12)/), &
         (/3,4/)))
    end function

    function createChild(a)
        class(Child(*,4,*,4)), intent(in) :: a
        class(Base(:,4)), allocatable :: createChild(:,:)
        allocate(createChild(4,5),SOURCE=reshape((/(Child(20,4,20,4)(i,-i), &
         i=1,20)/),(/4,5/)))
    end function
end module

program functionReturn002
use m
    class(Base(:,4)), allocatable :: a
    class(*), pointer :: v1(:)
    logical :: m1(20)

    allocate(Base(20,4)::a)
    allocate(v1(10), SOURCE=(/(Base(20,4)(i),i=101,110)/))
    m1 = (/.FALSE.,.TRUE.,.TRUE.,.FALSE.,.TRUE.,.TRUE.,.FALSE., &
     .FALSE.,.TRUE.,.FALSE.,.TRUE.,.FALSE.,.FALSE.,.FALSE.,.TRUE., &
     .FALSE.,.FALSE.,.TRUE.,.TRUE.,.FALSE./)

    select type(name1=>pack(a%create(), reshape(m1,(/3,4/)), v1))
        type is (Base(*,4))
            print *, name1
            print *, shape(name1)
        class default
            error stop 1_4
    end select

    deallocate(a, v1)
    allocate(Child(20,4,20,4)::a)
    allocate(v1(12), SOURCE=(/(Child(20,4,20,4)(i,-i),i=101,112)/))

    select type(name1=>pack(a%create(),reshape(m1,(/4,5/)), v1))
        type is (Child(*,4,*,4))
            print *, name1
            print *, shape(name1)
        class default
            error stop 2_4
    end select
end
