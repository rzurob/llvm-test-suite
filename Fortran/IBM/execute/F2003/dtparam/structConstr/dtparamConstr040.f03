
module m
    type A (k, n)
        integer, kind :: k
        integer, len :: n

        real(k) :: data(n)
        procedure(compareAB), pass(a1), pointer :: equal
    end type

    type B (k, n)
        integer, kind :: k
        integer, len :: n

        real(k) :: data(n)
        procedure(compareAB), pass(b1), pointer :: equal
    end type

    abstract interface
        logical function compareAB (a1, b1)
        import
            class(A(8,*)), intent(in) :: a1
            class(B(4,*)), intent(in) :: b1
        end function
    end interface
end module

program dtparamConstr040
use m
    procedure(compareAB) isABequal

    real(8) d1(800)

    type(A(8,:)), allocatable :: a1
    type(B(4,589)) :: b1

    b1 = B (4,589)(log((/(1.0*i, i=1,589)/)), equal = isABequal)

    d1 = (/(i, i=800,1,-1)/)

    d1 = log(d1)

    allocate (a1, source=A(8,589)(equal=isABequal, data=d1(800:212:-1)))

    if ((.not. a1%equal(b1)) .or. (.not. b1%equal(a1))) error stop 1_4

    deallocate (a1)

    allocate (A(8,200) :: a1)

    a1 = A(8,200)(d1(:200), isABequal)

    if (a1%equal(b1) .or. b1%equal(a1)) error stop 2_4
end

logical function isABequal (a1, b1)
use m
    class(A(8,*)), intent(in) :: a1
    class(B(4,*)), intent(in) :: b1

    logical(4), external :: precision_r4

    if (a1%n /= b1%n) then
        isABequal = .false.
    else
        do i = 1, a1%n
            isABequal = precision_r4(b1%data(i), real(a1%data(i), kind=4))

            if (.not. isABequal) return
        end do
    end if
end function
