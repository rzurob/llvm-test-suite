
module m
    type A (kk)
        integer, kind :: kk

        integer :: k
        real(kk) :: data
    end type

    type (A(8)), parameter :: a8_const = A(8)(8, 1.1d0)
    type (A(4)), parameter :: a4_const = A(4)(4, 1.0e0)

    type base
        integer :: id = -1
        type(A(a8_const%k)) :: a1 = a8_const
        class(A(a4_const%k)), allocatable :: a2(:)
    end type
end module
