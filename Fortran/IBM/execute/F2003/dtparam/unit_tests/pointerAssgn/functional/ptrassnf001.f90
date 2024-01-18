      type base (k, n)
        integer, kind :: k, n

        real(k) :: data(n) = 1.0
      end type

      type(base(4, 10)), pointer :: basePtr
      type(base(4, 10)), target :: baseTarget

      basePtr => baseTarget

      end
