      ! The VALUE attribute is allowed on pure procedures
      ! Test PURE and ELEMENTAL procedures.

      module m
        implicit none
      contains
        pure subroutine puresub1(a, b)
          implicit none
          integer, intent(out) :: a
          integer, value :: b
          a = a + b + 1
        end subroutine

        pure function purefun1(b)
          implicit none
          integer, value :: b
          integer purefun1
          purefun1 = b + 1
        end function

        elemental subroutine elemsub1(a, b)
          implicit none
          integer, intent(out) :: a
          integer, value :: b
          a = a + b - 1
        end subroutine

        elemental function elemfun1(b)
          implicit none
          integer, value :: b
          integer elemfun1
          elemfun1 = b - 1
        end function
      end module

      use m
      implicit none
      integer x
      integer y(4)

      x = purefun1(5)
      call puresub1(x, 7)
      if (x /= 14) then
        print *, x
        stop 1
      endif

      y = elemfun1([2, 3, 4, 5])
      call elemsub1(y, [7, 8, 9, 10])
      if (any(y /= [7, 9, 11, 13])) then
        print *, y
        stop 2
      endif

      end
