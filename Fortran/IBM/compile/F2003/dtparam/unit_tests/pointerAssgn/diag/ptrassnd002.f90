!*****************************************************************
!* DIAG: The 2nd type parameter does not match between the ptr
!*       and the target.
!*
!*****************************************************************

      type base (k, n)
        integer, kind :: k, n

        real(k) :: data(n) = 1.0
      end type

      type, extends(base) :: child (x, y)
        integer, kind :: x, y
        integer(x) :: arr(y) = 5
      end type

      class(base(4, 20)), pointer :: basePtr

      type(child(4, 10, 8, 30)), target :: childTarget

      basePtr => childTarget

      end
