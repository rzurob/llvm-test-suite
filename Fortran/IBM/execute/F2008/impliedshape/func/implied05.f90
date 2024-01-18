      ! F2008 Implied-shape arrays
      ! - Test high rank support.
      ! - Base type declared in implicit
      ! - BLOCK construct declared new implied shape array with
      !   the same name.

      implicit complex(8) (x-z)
      integer i
      integer, parameter :: sizes(*) = &
        [1, 1, 2, 2, 3, 3, 1, 1, 2, 2, 3, 3, 1, 1, 2]

      dimension :: x(  0:*,  -1:*,  -2:*,  -3:*,  -4:*, &
                      -5:*,  -6:*,  -7:*,  -8:*,  -9:*, &
                     -10:*, -11:*, -12:*, -13:*, -14:*)
      parameter (x = &
        reshape([((1.0d0, 2.0d0), i = 1, 2592)], sizes))

      do i = 1, 15
        block
          integer :: v_lbound
          integer :: v_ubound

          v_lbound = 1 - i
          v_ubound = v_lbound + sizes(i) - 1

          if (lbound(x, i) /= v_lbound) error stop 1
          if (ubound(x, i) /= v_ubound) error stop 2
          if (size(x, i) /= sizes(i)) error stop 3
        end block
      end do

      block
        dimension :: x(*)
        parameter (x = [(3.0d0, 4.0d0), (5.0d0, 6.0d0)])

        if (lbound(x, 1) /= 1) error stop 4
        if (ubound(x, 1) /= 2) error stop 5
        if (size(x, 1) /= 2) error stop 6
      end block

      end
