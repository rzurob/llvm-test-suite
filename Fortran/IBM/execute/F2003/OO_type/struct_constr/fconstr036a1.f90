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
    type dataType
    end type

    type container
        class (dataType), allocatable :: data1(:)
        class (dataType), allocatable :: data2(:,:)
    end type
end module

program fconstr036a1
use m
    type, extends(dataType) :: mainData
        integer(8) :: id
    end type

    type (container) :: co1(2)
    class (dataType), allocatable :: source(:)
    class (dataType), allocatable :: source2(:,:)

    !! test 1
    co1(1) = container (source, source2)

    if (allocated (co1(1)%data1) .or. allocated (co1(1)%data2)) error stop 1_4

    !! test 2
    allocate (source(0:2), source=(/mainData(1_8), mainData(10_8), &
                                    mainData(20_8)/))

    allocate (source2(2,0:1))

    co1(2) = container (source, source2)

    if (same_type_as (co1(2)%data1, co1(2)%data2)) error stop 2_4

    if ((lbound(co1(2)%data1, 1) /= 0) .or. (ubound(co1(2)%data1, 1) /= 2)) &
            error stop 3_4

    if (any (lbound(co1(2)%data2) /= (/1, 0/))) error stop 4_4

    if (any (ubound(co1(2)%data2) /= (/2,1/))) error stop 5_4

    select type (x => co1(2)%data1)
        type is (mainData)
            if (any (x%id /= (/1, 10, 20/))) error stop 6_4
        class default
            error stop 7_4
    end select

    select type (x => co1(2)%data2)
        type is (dataType)
        class default
            error stop 8_4
    end select
end
