@PROCESS INTLOG
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

        end interface

        logical(c_bool)      logc1
        logical(2)           logc2
        logical(4)           logc4
        logical(8)           logc8
        logical              logc
        logical, parameter :: logc_c = .false.

        real(c_float)       real1
        real(c_long_double) real2
        integer(c_int8_t)   int8_1, int8_2
        integer(c_int16_t)  int16
        integer(c_int32_t)  int32
        integer(c_int64_t)  int64

        integer(c_size_t) rt

        rt = c_sizeof(.true.)
        if (rt /= kind(logc_c)) error stop 1

        rt = c_sizeof(logc_c)
        if (rt /= kind(logc_c)) error stop 2

        rt = c_sizeof(logc2)
        if (rt /= kind(logc2)) error stop 3

        rt = c_sizeof(logc4)
        if (rt /= kind(logc4)) error stop 4

        rt = c_sizeof(logc8)
        if (rt /= kind(logc8)) error stop 5

        rt = c_sizeof(logc)
        if (rt /= kind(logc)) error stop 6

        rt = c_sizeof(int8_1 > int8_2)
        if (rt /= c_bool) error stop 7

        rt = c_sizeof(int8_1 /= int16)
        if (rt /= c_int16_t) error stop 8

        rt = c_sizeof(real2 < int8_1)
        if (rt /= c_long_double) error stop 9

        rt = c_sizeof(int8_1 < real2)
        if (rt /= c_long_double) error stop 10

        rt = c_sizeof(logc1 .EQV. logc2)
        if (rt /= 2) error stop 11

        rt = c_sizeof(logc2 .and. logc4)
        if (rt /= 4) error stop 12

        rt = c_sizeof(logc4 .or. logc8)
        if (rt /= 8) error stop 13

        rt = c_sizeof(.not. logc)
        if (rt /= kind(logc)) error stop 14

        rt = c_sizeof(func2()>func1(3))
        if (rt /= c_double) error stop 15
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
        func2 = 4
      end function
