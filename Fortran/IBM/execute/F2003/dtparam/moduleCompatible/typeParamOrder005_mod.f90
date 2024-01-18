module m
    type A (n, k)
        integer, kind :: k
        integer, len :: n

        real(k) :: data(n)
    end type

    abstract interface
        function f1(n) result(res)
        import
            integer, intent(in) :: n
            type(A(n,4)) res
        end function

        function f2(n) result(res)
        import
            integer, intent(in) :: n
            type(A(n,8)) res
        end function
    end interface

    type :: base (k, n, ka, na)
        integer, kind :: k, ka
        integer, len :: n, na

        complex(k) :: cx(n)
!        procedure(type(A(na, ka))), pointer, nopass :: p1 => null()
        procedure(f1), pointer, nopass :: p1 => null()
        procedure(f2), pointer, nopass :: p2 => null()
    end type

    contains

    !! the declaration of the dummy-arg b is unusual
    subroutine getBaseVal4_4 (cx, r1, b)
        complex(4), intent(inout) :: cx(:)
        real(4), intent(inout) :: r1(:)
        type (base(4,size(cx),4, size(r1))), intent(in) :: b

        type(A(size(r1),4)) :: a1_temp

        cx = b%cx

        if (associated(b%p1)) then
            a1_temp = b%p1(25)

            r1 = a1_temp%data
        end if
    end subroutine

    subroutine normalizeBaseVal8_8 (b, cx, d1)
        type (base(8,*,8,*)), intent(in) :: b
        complex(8), intent(inout) :: cx(b%n)
        double precision, intent(inout) :: d1(b%na)

        type (A(b%na, 8)) a1_temp

        real(8) denominator

        denominator = abs(sum(b%cx))

        cx = b%cx /denominator

        if (associated(b%p2)) then
            a1_temp = b%p2(30)

            denominator = abs(sum(a1_temp%data))

            d1 = a1_temp%data/denominator
        end if
    end subroutine
end module
