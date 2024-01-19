! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/23/2005
!*
!*  DESCRIPTION                : dtparam (section 4.5.6.1: inheritance)
!                               Statement: Additional type parameters may be
!                               declared in the definition of the extended type.
!                               Case: integer array with adjustable size.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type object
    end type
end module

module m1
use m
    type, extends(object) :: arrayTemplate (size)
        integer, len :: size
    end type

    type, extends(arrayTemplate) :: intArray (k)
        integer, kind :: k
        integer(k) data(size)
        real(k*2) sum           !<-- without dummy-arg support, use sum to hold
                                !  the sum() of the data
    end type
end module

program dtparamExtends007
use m1
    type (intArray (k=4, size=500)) ia1
    type (intArray (10000, k=8)) ia2

    logical(4) precision_r8, precision_r6

    ia1%data = (/(i, i = 1, ia1%size)/)
    ia2%data = (/(i*10, i = 1, ia2%size)/)

    ia1%sum = sum (ia1%data)
    ia2%sum = sum (ia2%data)

    !! verify data
    if (any(ia1%data(200:500) /= (/(i, i=200,500)/))) error stop 1_4
    if (any(ia2%data(300:1000:2) /= (/(i*10, i=300,1000,2)/))) error stop 2_4

    if (.not. precision_r8 (ia1%sum, 1.2525d5)) error stop 3_4
    if (.not. precision_r6 (ia2%sum, 5.0005q8)) error stop 4_4
end
