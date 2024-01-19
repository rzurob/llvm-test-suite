      type base (k, n)
        integer, kind :: k=4, n=10

        real(k) :: data(n) = 1.0
      end type

      type, extends(base) :: child (x, y)
        integer, kind :: x=8, y=20
        integer(x) :: arr(y) = 5
      end type

      class(base(4, 10)), pointer :: basePtr

      type(child(4, 8, 8, 20)), target :: childTarget

      basePtr => childTarget

      end
