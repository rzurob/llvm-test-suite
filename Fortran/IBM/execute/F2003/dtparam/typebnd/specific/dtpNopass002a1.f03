! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/27/2007
!*
!*  DESCRIPTION                : derived type parameter
!                               specific type bound (Test nopass binding for
!                               extended type: overriden binding: use non-poly
!                               objects to invoke the binding)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, n)
        integer, kind :: k
        integer, len :: n

        integer(k) :: ids(n)

        contains

        procedure, nopass :: typeInfo => baseTypeInfo
    end type

    type, extends(base) :: child (m)
        integer, len :: m

        character(m) :: name

        contains

        procedure, nopass :: typeInfo => childTypeInfo
    end type

    contains

    integer function baseTypeInfo()
        baseTypeInfo = 1
    end function

    integer function childTypeInfo ()
         childTypeInfo = 2
    end function
end module

module m1
use m, only: base, child
    type, extends(child) :: gen3
        logical(k) :: flag

        contains

        procedure, nopass :: typeInfo => gen3TypeInfo
    end type

    contains

    integer function gen3TypeInfo ()
        gen3TypeInfo = 3
    end function
end module


use m1
    type, extends(child) :: gen3_1
        integer(k) :: i
    end type

    type(gen3(4,15,20)) g1

    type(gen3(8,:,:)), allocatable :: g2(:)

    type (gen3_1(8,:,:)), pointer :: g3(:,:)

    type(child(8, 20, 20)) c2(100)

    allocate (gen3(8,100, 40) :: g2(100))

    allocate (g3(10,20), source=gen3_1(8,12,3)(1, 'abc', 2))

    if ((g1%typeInfo() /= 3) .or. (g1%child%typeInfo() /= 2) .or. &
        (g1%child%base%typeInfo() /= 1) .or. (g1%base%typeInfo() /= 1)) &
        error stop 1_4

    if ((c2%typeInfo() /= 2) .or. (c2(20)%base%typeInfo() /= 1)) error stop 2_4


    if ((g2%typeInfo() /= 3) .or. (g2%child%typeInfo() /= 2) .or. &
        (g2(:)%child%base%typeInfo() /= 1) .or. (g2(23)%base%typeInfo() /= 1)) &
        error stop 3_4

    if ((g3%typeInfo() /= 2) .or. (g3%base%typeInfo() /= 1) .or. &
        (g3%child%typeInfo() /= 2)) error stop 4_4
end
