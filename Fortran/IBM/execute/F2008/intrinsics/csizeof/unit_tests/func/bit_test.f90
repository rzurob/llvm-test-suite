      program t
        use, intrinsic :: iso_c_binding
        implicit none
        integer default_int
        
        integer(c_size_t) rt

        type(c_ptr) pd
        type(c_funptr) pf
        integer(c_long) long_64bit
        integer(c_int_fast16_t) int_fast16
        integer(c_int_fast32_t) int_fast32

        rt = c_sizeof(long_64bit)
        if (rt /= c_long) then
          print *, rt
          error stop 1
        endif

        rt = c_sizeof(pd)
        if (rt /= c_long) then
          print *, rt
          error stop 2
        endif

        rt = c_sizeof(pf)
        if (rt /= c_long) then
          print *, rt
          error stop 3
        endif

        rt = c_sizeof(rt)
        if (rt /= c_size_t) then
          print *, rt
          error stop 4
        endif

        rt = c_sizeof(c_sizeof(rt))
        if (rt /= c_size_t) then
          print *, rt
          error stop 5
        endif

        rt = c_sizeof(int_fast16)
        if (rt /= c_int_fast16_t) then
          print *, rt
          error stop 6
        endif

        rt = c_sizeof(int_fast32)
        if (rt /= c_int_fast32_t) then
          print *, rt
          error stop 7
        endif

        end
