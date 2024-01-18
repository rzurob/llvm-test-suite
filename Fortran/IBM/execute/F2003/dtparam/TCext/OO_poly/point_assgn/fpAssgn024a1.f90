! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qnodeferredlp -qreuse=base /tstdev/OO_poly/point_assgn/fpAssgn024a1.f
! opt variations: -qck -qnok -qnol -qdefaultpv -qdeferredlp -qreuse=self -qreuse=none

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/26/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (non-poly pointer array
!*                               assigned to poly-pointer array through
!*                               structure constructor; rank-one arrays)
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
    type base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)   :: id = 0
    end type

    type, extends (base) :: child    ! (20,4)
        character(n1) :: name = ''
    end type

    type (child(20,4)), target, allocatable :: c1_m(:)
end module

module m1
use m
    type container(k2,n2)    ! (4,20)
        integer, kind              :: k2
        integer, len               :: n2
        type(base(n2,k2)), pointer :: data(:) => null()
    end type

    type (container(4,20)), allocatable :: co1_m
end module

program fpAssgn024a1
use m1
    type (child(20,4)), target :: c1(10), c2 (4, 4)

    class (base(20,4)), pointer :: b_ptr (:)

    allocate (co1_m, c1_m(20))


    !! assign co1_m%data to c1 through b_ptr in struct_constr
    b_ptr => c1

    co1_m = container(4,20) (data = b_ptr)

    if ((.not. associated (co1_m%data, c1%base)) .or. &
        (size (co1_m%data) /= 10)) error stop 1_4

    co1_m%data(::2) = (/(base(20,4)(id = i), i=1,10,2)/)
    co1_m%data(2::2) = (/(base(20,4)(id = -i), i=2,10,2)/)

    !! verify c1
    do i =1, 10
        if (c1(i)%id /= (-1)**(i+1)*i) error stop 2_4
    end do



    !! assign co1_m%data to c1_m(::2)%base through b_ptr in struct_constr
    b_ptr => c1_m

    co1_m = container(4,20) (data = b_ptr(::2))

    if ((.not. associated (co1_m%data, c1_m(::2)%base)) .or. &
        (size (co1_m%data) /= 10)) error stop 3_4

    co1_m%data = (/(base(20,4)(i), i=1,10)/)

    !! verify c1_m
    do i = 1, 20, 2
        if (c1_m(i)%id /= (i+1)/2) error stop 4_4
    end do



    !! assign co1_m%data to c2(3,::2)%base through b_ptr in struct_constr
    b_ptr => c2 (3,:)

    co1_m = container(4,20) (data = b_ptr(::2))

    if ((.not. associated (co1_m%data, c2(3,::2)%base)) .or. &
        (size (co1_m%data) /= 2)) error stop 5_4

    co1_m%data(1)%id = -10
    co1_m%data(2)%id = -20

    if ((c2(3,1)%id /= -10) .or. (c2(3,3)%id /= -20)) error stop 6_4
end
