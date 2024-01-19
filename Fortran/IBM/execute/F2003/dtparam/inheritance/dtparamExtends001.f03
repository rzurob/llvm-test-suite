! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/22/2005
!*
!*  DESCRIPTION                : dtParameter (Section 4.5.6.1, stmt: An extended
!                               type includes all of the type parameters of its
!                               parent.
!                               Case: parent has no type parameter
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer(4) :: id = 0
    end type

    type, extends(base) :: child (k, l)
        integer, kind :: k = 4
        integer(min(k,8)), len :: l = 1
        real(k) data(l)
    end type

    type (child(l=10)), save :: c1_m
end module

program dtparamExtends001
use m
    type (child(k=8)) c1
    type (child) c2
    type (child(l=30, k=16)) c3
    type (child(8, 15)) c4

    !! verify the type parameter values
    if ((c1_m%k /= 4) .or. (c1_m%l /= 10)) error stop 1_4
    if ((kind(c1_m%l) /= 4) .or. (size(c1_m%data) /= 10))  error stop 2_4

    if ((c1%k /= 8) .or. (c1%l /= 1)) error stop 3_4
    if ((kind(c1%l) /= 8) .or. (size(c1%data) /= 1)) error stop 4_4

    if ((c2%k /= 4) .or. (c2%l /= 1)) error stop 5_4
    if ((kind(c2%l) /= 4) .or. (size(c2%data) /= 1)) error stop 6_4

    if ((c3%k /= 16) .or. (c3%l /= 30)) error stop 7_4
    if ((kind(c3%l) /= 8) .or. (size(c3%data) /= 30)) error stop 8_4

    if ((c4%k /= 8) .or. (c4%l /= 15)) error stop 9_4
    if ((kind(c4%l) /= 8) .or. (size(c4%data) /= 15)) error stop 10_4
end
