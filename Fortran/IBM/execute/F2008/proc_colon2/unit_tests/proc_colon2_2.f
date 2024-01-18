      ! F2008 allows :: in the procedure statement in interface blocks
      ! Check that we don't incorrectly langlvl flag other instances of
      ! procedure followed by ::.
      module m
        type dt
          integer i
        contains
          procedure :: print  ! OK in F2003
        end type

        abstract interface
          function foo(i)
            integer foo, i
            value :: i
          end function
        end interface
      contains
        subroutine print(x)
          class(dt) x
          print *, x%i
        end subroutine

        subroutine sub(x, y)
          procedure(foo) :: x ! OK in F2003
          procedure(foo) :: y ! OK in F2003
          integer i

          i = x(3) + y(4)
          if (i /= 7) error stop 1_4
        end subroutine

        integer function add1(i)
          integer, value :: i
          add1 = i + 1
        end function

        integer function sub1(i)
          integer, value :: i
          sub1 = i - 1
        end function
      end module

      use m
      call sub(add1, sub1)
      end
