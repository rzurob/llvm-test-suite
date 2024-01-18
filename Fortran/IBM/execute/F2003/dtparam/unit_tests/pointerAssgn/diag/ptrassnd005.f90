!*****************************************************************
!* Diag: Mismatch type parameters.
!*
!*****************************************************************

      type base (k, n)
        integer, kind :: k, n

        real(k) :: data(n) = 1.0
      end type
 
      type, extends(base) :: child (x, y, p, q)
        integer, kind :: x, y
        integer, len :: p, q 
        integer(x) :: arr(p) = 5
      end type

      class(base(4, 20)), pointer :: basePtr

      type(child(4, 10, 8, 30, 30, 50)), pointer :: childPtr
      type(child(4, 10, 8, 30, 40, 50)), target :: childTarget

      childPtr => childTarget
      basePtr => childTarget

      end
