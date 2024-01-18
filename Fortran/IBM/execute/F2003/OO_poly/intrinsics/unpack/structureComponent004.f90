!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/unpack
! PROGRAMMER                 : Yong Du
! DATE                       : 02/25/2005
! PRIMARY FUNCTIONS TESTED   : unpack
! DRIVER STANZA              : xlf90
! DESCRIPTION                : VECTOR or FIELD is a structure component,
!                              which is a scalar. The object containing
!                              the component is an array. Non-poly.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type, abstract :: AbstractParent
    end type

    type, extends(AbstractParent) :: Base
        integer i
    end type

    type, extends(Base) :: Child
        type(Base) :: b1
        type(Base) :: v1
    end type
end module

program structureComponent004
use m
    type(Child) :: c1(4,5)
    type(Child) :: c2(12)

    c1 = reshape((/(Child(i,Base(-i),Base(i)),i=101,120)/), (/4,5/))
    c2 = (/(Child(i,Base(-i),Base(i)),i=1,12)/)

    associate(name1=>unpack(c2%v1, MOD(c1%i,2)==1, c1%b1))
        if(.NOT. same_type_as(name1, Base(1))) error stop 1_4
        print *, name1
        print *, shape(name1)
    end associate
end
