!******************************************************************
!* DIAG: deferred length type parameter must match between
!*       an actual argument and a dummy argument.
!*
!******************************************************************

      type base (k, n)
        integer, kind :: k
        integer, len :: n

        real(k) :: data(n) = 1.0
      end type

      type(base(4, 10)), pointer :: basePtr
      type(base(4, 20)), target :: baseTarget

      call sub(basePtr, baseTarget)

      contains

      subroutine sub(argPtr, argTarget)
        type(base(4, :)), pointer :: argPtr
        type(base(4, *)), target :: argTarget

        argPtr => argTarget
      end subroutine

      end
