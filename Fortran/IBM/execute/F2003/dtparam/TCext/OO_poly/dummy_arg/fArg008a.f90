! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg008a.f
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
    type data1(k1)    ! (4)
        integer, kind :: k1
        real(k1)      :: data

        contains

        procedure, pass (b1) :: greaterThanSum => b1GTb2Plusb3
    end type

    type data2(k2)    ! (8)
        integer, kind :: k2
        real(k2)      :: data

        contains

        procedure, pass (b2) :: lessThanDiff => b1GTb2Plusb3
    end type

    type data3(k3)    ! (4)
        integer, kind :: k3
        integer(k3)   :: data

        contains

        procedure, pass (b3) :: lessThanDiff => b1GTb2Plusb3
    end type

    contains

    logical function b1GTb2Plusb3 (b1, b2, b3)
        class (data1(4)), intent(in) :: b1
        class (data2(8)), intent(in) :: b2
        class (data3(4)), intent(in) :: b3

        b1GTb2Plusb3 = (b1%data > (b2%data + b3%data))
    end function
end module

program fArg008a
use m
    type (data1(4)) :: c = data1(4) (10.5)

    class (data2(8)), allocatable :: a
    class (data3(4)), pointer :: b

    allocate (b,a)

    a%data = 4.0
    b%data = 6

    if (.not. c%greaterThanSum (a, b)) error stop 1_4

    if (.not. a%lessThanDiff (c, b)) error stop 2_4

    if (.not. b%lessThanDiff (c, a)) error stop 3_4

    if (c%greaterThanSum (data2(8)(4.8), b)) error stop 4_4

    if (b%lessThanDiff (data1(4)(10.0), data2(8)(4.5))) error stop 5_4

    if (a%lessThanDiff (c, data3(4) (7))) error stop 6_4

    deallocate (b)
end
