!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/03/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.9: structure constr)
!                               Case: C490: proc target for the proc-pointer
!                               component.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, n)
        integer, kind :: k
        integer, len :: n

        procedure(real(k)), nopass, pointer :: p1
        real(k) :: data(n)
    end type

    type (base(8,:)), allocatable :: b1
end module

program dtparamConstr021
use m
    type (base(4, 21)) b2

    real(4), pointer :: r1
    real(8), pointer :: d1

    procedure(real(4)), pointer :: r4Ptr
    procedure(real(8)), pointer :: r8Ptr

    procedure(real(4)) sumAll
    procedure(real(8)) sumSqrt

    logical(4), external :: precision_r4, precision_r8

    r4Ptr => sumAll
    r8Ptr => sumSqrt

    allocate (base(8,45) :: b1)

    b1 = base(8,45)(data=(/((i-20)*1.0d0, i=1,45)/), p1=r8Ptr)

    b2 = base(4,21)(data=1.0, p1=r4Ptr)

    allocate (r1, source=b2%p1(21, b2%data))
    allocate (d1, source=b1%p1(45, b1%data))

    !! verify
    if (.not. precision_r4 (r1, 2.1e1)) error stop 1_4

    if (.not. precision_r8 (d1, 1.428276221314983d2)) error stop 2_4
end


real(4) function sumAll (i, r1)
    integer, intent(in) :: i
    real(4), intent(in) :: r1(i)

    sumAll = sum(r1)
end function

real(8) function sumSqrt (i, d1)
    integer, intent(in) :: i
    real(8), intent(in) :: d1(i)

    sumSqrt = sum (sqrt(abs(d1)))
end function
