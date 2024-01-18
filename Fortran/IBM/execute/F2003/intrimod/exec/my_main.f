!... This main program uses my_mod and call sub1 from that.

      program my_main

         use ieee_arithmetic
	 use ieee_exceptions
         use xlf_fp_util
         use my_mod

!...  Calling non_intrinsic module procedure
         call sub1()
         call sub2()
      end
