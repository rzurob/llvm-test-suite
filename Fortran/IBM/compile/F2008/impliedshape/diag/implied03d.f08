      ! F2008 implied-shape arrays -- Diagnostics

      subroutine sub(n)
        integer n

        ! Ubound must be *
        integer, parameter :: a(*, 2, *) = reshape([1, 2, 3, 4, 5, 6, 7, 8], [2, 2, 2])
        integer, parameter :: b(2, 2:*) = reshape([1, 2, 3, 4], [2, 2])
        integer, parameter :: c(:, *) = reshape([1, 2, 3, 4], [2, 2])
        integer, parameter :: d(2:n, *) = reshape([1, 2, 3, 4], [2, 2])

        ! stride must not be provided
        integer, parameter :: e(2:*:1) = [1, 2, 3]

     end subroutine
