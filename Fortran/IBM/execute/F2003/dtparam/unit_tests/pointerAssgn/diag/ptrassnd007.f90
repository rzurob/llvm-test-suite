      type base (k, n)
        integer, kind :: k, n

        real(k) :: data(n) = 1.0
      end type
 
      type, extends(base) :: child (x, y)
        integer, kind :: x
        integer, len :: y 
      end type

      type, extends(child) :: gchild (p, q)
        integer, kind :: p 
        integer, len :: q 
      end type

      class(base(4, 20)), pointer :: basePtr

      class(child(4, 10, 8, 30)), pointer :: childPtr
      type(child(4, 10, 8, 30)), target :: childTarget

      type(gchild(4, 20, 8, 30, 40, 50)), pointer :: gchildPtr
      type(gchild(4, 10, 8, 40, 40, 50)), target :: gchildTarget

      basePtr => childTarget
      basePtr => gchildTarget

      childPtr => gchildTarget
      gchildPtr => gchildTarget

      end
