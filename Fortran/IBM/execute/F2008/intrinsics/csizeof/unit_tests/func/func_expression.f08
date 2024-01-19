      program t
        use, intrinsic :: iso_c_binding
        implicit none

        interface
          real(c_float) function func1(i)
            use, intrinsic :: iso_c_binding
            implicit none
            integer i
          end function

          real(c_double) function func2()
            use, intrinsic :: iso_c_binding
            implicit none
          end function

          function func3()
            use, intrinsic :: iso_c_binding
            integer(c_long_long) func3(3)
          end function
        end interface

        integer(c_size_t) rt

        integer(c_int8_t) :: int1, int2
        real(c_float) :: real1
        real(c_long_double) :: real2
        integer(c_long_long) iarr(4)
        character(1) str1
        character(0) str0

        ! expression with logical type is tested in logical.f
        int1=1;int2=2
        real1=4.5;real2=5.4

        rt = c_sizeof(int1)
        if (rt /= c_int8_t) error stop 1

        rt = c_sizeof(real1+int2)
        if (rt /= c_float) error stop 2

        rt = c_sizeof(real2+int1)
        if (rt /= c_long_double) error stop 3

        rt = c_sizeof(int1+real1)
        if (rt /= c_float) error stop 4

        rt = c_sizeof(func1(3))
        if (rt /= c_float) error stop 5

        rt = c_sizeof(func1(3)+func2())
        if (rt /= c_double) error stop 6

        rt = c_sizeof(iarr*kind(iarr))
        if (rt /= c_long_long * size(iarr)) error stop 7

        rt = c_sizeof(2-iarr)
        if (rt /= c_long_long * size(iarr)) error stop 8

        rt = c_sizeof(iarr(2:4)*kind(iarr))
        if (rt /= c_long_long * (4 - 2 + 1)) error stop 9

        rt = c_sizeof(kind(iarr)*iarr(2:4))
        if (rt /= c_long_long * (4 - 2 + 1)) error stop 10

        rt = c_sizeof(2*iarr(1:2))
        if (rt /= c_long_long * (2 - 1 + 1)) error stop 11

        rt = c_sizeof(3+func3())
        if (rt /= c_long_long * 3) error stop 12

        rt = c_sizeof(func3()-3)
        if (rt /= c_long_long * 3) error stop 13

        rt = c_sizeof(""//"b")
        if (rt /= 1) error stop 14

        rt = c_sizeof("a"//"")
        if (rt /= 1) error stop 15

        rt = c_sizeof(str1//str0)
        if (rt /= 1) error stop 16

        call sub0()
      end

      real(c_float) function func1(i)
        use, intrinsic :: iso_c_binding
        implicit none
        integer i
        func1 = 1.0+i
      end function

      real(c_double) function func2()
        use, intrinsic :: iso_c_binding
        implicit none
        func2=4
      end function

      function func3()
        use, intrinsic :: iso_c_binding
        integer(c_long_long) func3(3)
        func3 = 1
      end function

      subroutine sub0() ! expression and function reference
      use, intrinsic :: iso_c_binding
      implicit none
        interface
          function func11()
            use, intrinsic :: iso_c_binding
            integer(c_long_long), pointer, dimension(:) :: func11
          end function

          function func12()
          use, intrinsic :: iso_c_binding
          integer(c_long_long), pointer, dimension(:, :) :: func12
          end function
        end interface

        integer(c_size_t) :: rt

        rt = c_sizeof(2+func11())
        if (rt /= c_long_long * 10) error stop 17

        rt = c_sizeof(func11()-3)
        if (rt /= c_long_long * 10) error stop 18

        rt = c_sizeof(2*func12())
        if (rt /= c_long_long * 3 * 6) error stop 19

        rt = c_sizeof(func12()-3)
        if (rt /= c_long_long * 3 * 6) error stop 20
      end subroutine

      function func11()
      use, intrinsic :: iso_c_binding
      integer(c_long_long), pointer, dimension(:) :: func11
      allocate(func11(10))
      func11 = 11
      end function

      function func12()
      use, intrinsic :: iso_c_binding
      integer(c_long_long), allocatable, dimension(:, :) :: func12
      allocate(func12(3, 6))
      func12(:, :) = 12
      end function
