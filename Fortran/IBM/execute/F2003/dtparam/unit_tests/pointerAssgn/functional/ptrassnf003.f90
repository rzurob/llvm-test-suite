      type base (k, n, t)
        integer, kind :: k, n
        integer, len :: t

        real(k) :: data(n) = 1.0
        integer :: arr(t)
      end type

      type(base(4, 20, :)), pointer :: basePtr
      type(base(t=10, k=4, n=20)), target :: baseTarget

      baseTarget%arr = 5

      basePtr => baseTarget

      do ii = 1, 10
        if (basePtr%arr(ii) .ne. 5) stop 1
      end do

      basePtr%arr = 6 

      do ii = 1, 10
        if (basePtr%arr(ii) .ne. 6) stop 2
      end do

      if (size(basePtr%arr) .ne. 10) stop 3

      end
