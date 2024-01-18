! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr036a1.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/18/2005
!*
!*  DESCRIPTION                : structure constructor (poly-allocatable
!                               components' allocation in structure constructor)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type dataType(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
    end type

    type container(k2,n2)    ! (4,20)
        integer, kind                       :: k2
        integer, len                        :: n2
        class(dataType(k2,n2)), allocatable :: data1(:)
        class(dataType(k2,n2)), allocatable :: data2(:,:)
    end type
end module

program fconstr036a1
use m
    type, extends(dataType) :: mainData(k3)    ! (4,20,8)
        integer, kind :: k3
        integer(k3)   :: id
    end type

    type (container(4,20)) :: co1(2)
    class (dataType(4,20)), allocatable :: source(:)
    class (dataType(4,20)), allocatable :: source2(:,:)

    !! test 1
    co1(1) = container(4,20) (source, source2)

    if (allocated (co1(1)%data1) .or. allocated (co1(1)%data2)) error stop 1_4

    !! test 2
    allocate (source(0:2), source=(/mainData(4,20,8)(1_8), mainData(4,20,8)(10_8), &
                                    mainData(4,20,8)(20_8)/))

    allocate (source2(2,0:1))

    co1(2) = container(4,20) (source, source2)

    if (same_type_as (co1(2)%data1, co1(2)%data2)) error stop 2_4

    if ((lbound(co1(2)%data1, 1) /= 0) .or. (ubound(co1(2)%data1, 1) /= 2)) &
            error stop 3_4

    if (any (lbound(co1(2)%data2) /= (/1, 0/))) error stop 4_4

    if (any (ubound(co1(2)%data2) /= (/2,1/))) error stop 5_4

    select type (x => co1(2)%data1)
        type is (mainData(4,*,8))
            if (any (x%id /= (/1, 10, 20/))) error stop 6_4
        class default
            error stop 7_4
    end select

    select type (x => co1(2)%data2)
        type is (dataType(4,*))
        class default
            error stop 8_4
    end select
end
