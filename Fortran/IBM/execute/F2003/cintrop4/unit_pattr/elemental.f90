!*********************************************************************
!***********************************************************************

      elemental function f1 (a1)  bind(c)
        intent(in) :: a1

        f1 = a1 + 1
      end function

      program main
        dimension a(5)

        a = 1
        print*, a

        a = f1 (a)
        print*, a
      end
