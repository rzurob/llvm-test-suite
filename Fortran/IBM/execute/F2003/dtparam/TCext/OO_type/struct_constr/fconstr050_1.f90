! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr050_1.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/15/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (non-poly allocatable
!                               component with poly-entites as the data-source;
!                               use scalars and array elements)
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
    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)      id

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child(k2,n1)    ! (4,1,15)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1)    name

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printBase (b)
        class (base(4)), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child(4,1,*)), intent(in) :: b

        print *, b%id, b%name
    end subroutine
end module

module m1
use m
    type container(k3)    ! (4)
        integer, kind               :: k3
        type(base(k3)), allocatable :: data
    end type
end module

program fconstr050_1
use m1
    type (container(4)) :: co1, co2
    class (base(4)), allocatable :: b1(:), b2(:,:)

    allocate (b1(-1:0), source=(/child(4,1,15)(1, 'test1'), child(4,1,15)(2, 'test2')/))

    allocate (b2(0:1, -1:0), source=child(4,1,15)(10, 'test10'))

    !! in an associate construct
    associate ( x => container(4) (b1(0)), y => container(4) (b2(0,0)))
        call x%data%print
        call y%data%print
    end associate

    !! involve intrinsic assignments
    co1 = container(4) (b1(-1))

    co2 = container(4) (b2(1,0))

    call co1%data%print

    call co2%data%print
end

