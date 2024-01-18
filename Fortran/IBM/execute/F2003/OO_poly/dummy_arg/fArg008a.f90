! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/06/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (passed-object dummy-arg)
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
    type data1
        real*4 :: data

        contains

        procedure, pass (b1) :: greaterThanSum => b1GTb2Plusb3
    end type

    type data2
        real*8 :: data

        contains

        procedure, pass (b2) :: lessThanDiff => b1GTb2Plusb3
    end type

    type data3
        integer*4 :: data

        contains

        procedure, pass (b3) :: lessThanDiff => b1GTb2Plusb3
    end type

    contains

    logical function b1GTb2Plusb3 (b1, b2, b3)
        class (data1), intent(in) :: b1
        class (data2), intent(in) :: b2
        class (data3), intent(in) :: b3

        b1GTb2Plusb3 = (b1%data > (b2%data + b3%data))
    end function
end module

program fArg008a
use m
    type (data1) :: c = data1 (10.5)

    class (data2), allocatable :: a
    class (data3), pointer :: b

    allocate (b,a)

    a%data = 4.0
    b%data = 6

    if (.not. c%greaterThanSum (a, b)) error stop 1_4

    if (.not. a%lessThanDiff (c, b)) error stop 2_4

    if (.not. b%lessThanDiff (c, a)) error stop 3_4

    if (c%greaterThanSum (data2(4.8), b)) error stop 4_4

    if (b%lessThanDiff (data1(10.0), data2(4.5))) error stop 5_4

    if (a%lessThanDiff (c, data3 (7))) error stop 6_4

    deallocate (b)
end
