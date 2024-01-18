! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/03/2006
!*
!*  DESCRIPTION                : dtparam (section 16.2: scope of the type-param)
!                               Type parameters does not affect host
!                               associations in derived type definition: named
!                               const, derived types, abstract interface, module
!                               procedure and intrinsic procedures.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type A
        integer id
    end type

    type(A), parameter :: a_const = A(100)

    abstract interface
        function genA (id)
        import A
            type(A) genA
            integer, intent(in) :: id
        end function

        real function rsin(x)
            real, intent(in) :: x
        end function
    end interface

    type base (k, l)
        integer, kind :: k
        integer, len :: l

        integer(k) :: id(l)

        type(A) :: a1 = a_const
        procedure(rsin), pointer, nopass :: rsin
        procedure(genA), pointer, nopass :: genA
        procedure(genA1), pointer, nopass :: genA1
    end type

    contains

    type(A) function genA1()
        genA1%id = -1
    end function
end module

program dtparamScope002
use m
    procedure(genA) createA
    procedure(genA1) createA1
    procedure(rsin) mySineFunc

    type (base(8, 10)) b1
    type (base(4, 20)), allocatable :: b2
    type (A) a1(4)

    integer(8), parameter :: factor = 2**29
    logical(4), external :: precision_r4

    allocate (b2)

    b1%id = (/(i*factor, i=1, 10)/)
    b2%id = (/(i, i=1, 20)/)

    b1%genA => createA
    b1%genA1 => genA1
    b1%rsin => sin

    nullify(b2%genA)
    b2%genA1 => createA1
    b2%rsin => mySineFunc

    a1(1) = b1%genA(100)
    a1(2) = b1%genA1()

    a1(4) = b2%genA1()

    !! verify the results
    if (any(b1%id(::3)*10_8/factor /= (/10, 40, 70, 100/))) error stop 1_4
    if (any(b2%id /= (/(i, i=1, 20)/))) error stop 2_4

    if ((b1%a1%id /= 100) .or. (b2%a1%id /= 100)) error stop 3_4

    if (a1(1)%id /= 100) error stop 4_4
    if (a1(2)%id /= -1) error stop 5_4
    if (a1(4)%id /= 1) error stop 6_4

    if (.not. precision_r4(b1%rsin(.9e0), b2%rsin(0.9e0))) error stop 7_4
    if (.not. precision_r4(b1%rsin(.561e0), b2%rsin(0.561e0))) error stop 8_4
end

type(A) function createA (i)
use m, only: A
    integer, intent(in) :: i

    createA%id = i
end function

type(A) function createA1 ()
use m, only: A

    createA1%id = 1
end function

!! user defined sine function for abs(x) < 1
!! use Taylor series
real function mySineFunc (x)
    real, intent(in) :: x

    real newResult, dx, delta
    integer n, sign

    newResult = 0.0e0
    delta = 1.e-6

    n = 0
    sign = -1

1   n = n + 1

    dx = sign**(n-1) * x**(2*n-1)/factorial(2*n-1)

    mySineFunc = newResult + dx

    if (abs(mySineFunc - newResult) > delta) then
        newResult = mySineFunc
        goto 1
    end if

    contains

    integer function factorial (n)
        integer, intent(in) :: n

        integer array(n)

        array = (/(i, i=1,n)/)

        factorial = product(array)
    end function
end function
