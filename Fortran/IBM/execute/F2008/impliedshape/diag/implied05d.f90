      ! F2008 implied-shape arrays -- Diagnostics

      ! Attributes

      integer, save, dimension(*) :: a = [1, 2, 3]

      allocatable :: b
      integer, parameter :: b(*, *) = reshape([1, 1, 2, 2], [2, 2])

      end
