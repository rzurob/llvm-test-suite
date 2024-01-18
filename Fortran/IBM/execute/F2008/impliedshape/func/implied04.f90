      ! F2008 implied-shape arrays
      ! - Array of parameterized derived type objects
      ! - lbound is a specification expression (constant)
      module m
        implicit none
        type dt(k, l)
          integer, kind :: k
          integer, len :: l
          integer(k) i(l+1)
        end type
        type(dt(4, 3)), parameter :: x = dt(4, 3)([1, 2 ,3, 4])
      contains
        subroutine sub
          integer i
          type(dt(4, 3)), parameter :: y(2:*) = [ (x, i = 1, 2) ]
          print *, lbound(y, 1), shape(y)
          block
            type(dt(4, 3)), parameter :: z(lbound(y, 1):*) = [ (x, i = 1, 3) ]
            print *, lbound(z, 1), shape(z)
          end block
        end subroutine
      end module

      use m
      call sub
      end
