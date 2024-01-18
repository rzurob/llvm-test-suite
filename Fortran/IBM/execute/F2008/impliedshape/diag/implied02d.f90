      ! F2008 implied-shape arrays -- Diagnostics

      ! Test implied-shape array in the common statement
      integer a(*)
      common /blk/ a
      parameter(a = [2, 3])

      integer b(*, 2:*)
      common /blk2/ b
      parameter(b = reshape([1, 1, 2, 2], [2, 2]))

      ! Test implied-shape array in equivalence statements
      integer, parameter :: c(*) = [1, 2, 3, 4]
      integer, parameter :: d(*, *) = reshape([1, 2, 3, 4], [2, 2])
      equivalence(c, d)

      end
