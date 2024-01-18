       program c_ptrdiff_t_value
       use, intrinsic :: iso_c_binding
       implicit none
       integer(c_ptrdiff_t) x
       if (kind(x) /= c_long) then
         print *, kind(x)
         error stop 1
       end if
       end
