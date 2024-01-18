      subroutine check_c_to_f(c_arg2,c_len,test_no) bind(c)
         use, intrinsic :: iso_c_binding
         character(1), CONTIGUOUS :: c_arg2(..)
         integer(C_INT) c_len, test_no
         integer kk
         call _xlfdmprtd(c_arg2, 1)
         kk = rank(c_arg2)
         print*, kk
      end subroutine
