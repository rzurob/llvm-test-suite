      type base (k, n, t)
        integer, kind :: k, n
        integer, len :: t

        real(k) :: data(n) = 1.0
        integer :: arr(n, t)
      end type

      type(base(4, 20, :)), pointer :: basePtr
      type(base(t=10, k=4, n=20)), target :: baseTarget

      baseTarget%arr = 5

      basePtr => baseTarget

      print*, lbound(basePtr%arr) 
      print*, ubound(basePtr%arr)

      end
