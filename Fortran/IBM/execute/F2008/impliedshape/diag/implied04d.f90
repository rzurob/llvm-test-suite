      ! F2008 implied-shape arrays -- Diagnostics

      ! implied-shape arrays require the parameter attribute
      integer :: a(*) = [2, 3]

      dimension b(*, 2:*)
      integer :: b = reshape([1, 1, 2, 2], [2, 2])

      dimension c(*, 2:*)
      real c
      parameter(c = reshape([1.0,2.0,2.0], [1,2]))
      parameter(c = [3.0, 4.0, 4.0])

      end
