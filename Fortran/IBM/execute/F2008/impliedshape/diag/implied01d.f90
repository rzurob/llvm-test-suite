      ! F2008 implied-shape arrays -- Diagnostics

      ! Rank of initialization expressions does not equal rank of the array
      integer, parameter :: a(*) = reshape([1, 2, 3, 4], [2, 2])
      integer, parameter :: b(*, 2:*) = [3, 4]
      integer, parameter :: c(*, *, *) = 3
      integer, parameter :: d(*, *, *) = reshape([1, 2, 3, 4, 5, 6], [2, 2])

      end
