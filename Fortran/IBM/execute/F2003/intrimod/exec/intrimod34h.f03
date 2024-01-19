! ************************************************************************
!************************************************************************
!*
!*  Created By       : Bahram Chehrazy
!*  DATE             : January, 2004
!*  Description      : Use INTRINSIC module with the same name CRITICAL
!*                     SECTION. No error message should be issued.
!*                     -qextname is also used.
!*
!*************************************************************************
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  01/23/04   BC     Initial Version
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890
!


       module mod1

       contains
            subroutine sub2()
               use, intrinsic :: ieee_arithmetic
               use, intrinsic :: ieee_exceptions
               use, intrinsic :: xlf_fp_util

               real*4 yr, rr
               type(ieee_round_type), parameter :: rt_nearest = IEEE_NEAREST
               type(ieee_status_type) :: status_value
               integer(fpscr_kind), dimension(5) :: flags

               flags = (/ fp_overflow, fp_div_by_zero, fp_invalid, &
     &               fp_underflow, fp_inexact /)

               call ieee_get_status(status_value)
               call ieee_set_rounding_mode(rt_nearest)

!... Testing CRITICAL section with same name as an INTRINSIC module

               yr = 0.
!SMP$  PARALLEL DO private(rr)
               do i = 1, 100
                   rr = log(real(i))
!SMP$  CRITICAL (ieee_arithmetic)
                   yr = yr + ieee_rint(rr)
!SMP$  END CRITICAL (ieee_arithmetic)
               end do
!SMP$  END PARALLEL DO

                 print *, yr
               if (yr /= 360.0) error stop 14
               call ieee_set_status(status_value)
               call set_fpscr_flags(flags(1))
               if ( get_fpscr_flags(flags(1)) .eq. 0 ) error stop 15
            end subroutine sub2

       end module


       program intrimod34h

         use mod1
         logical fun1

         call sub1()
         call sub2()
         if ( .not. fun1() ) error stop 20

         contains

            subroutine sub1()
  	       use :: ieee_arithmetic
	       use :: ieee_exceptions
               use :: xlf_fp_util

               real*4 yr, rr
               type(ieee_round_type), parameter :: rt_nearest = IEEE_NEAREST
               type(ieee_status_type) :: status_value
               integer(fpscr_kind), dimension(5) :: flags

               flags = (/ fp_overflow, fp_div_by_zero, fp_invalid, &
     &               fp_underflow, fp_inexact /)

               call ieee_get_status(status_value)
               call ieee_set_rounding_mode(rt_nearest)

!... Testing CRITICAL section with same name as an INTRINSIC module

               yr = 0.
!SMP$  PARALLEL DO private(rr)
               do i = 1, 100
                   rr = log(real(i))
!SMP$  CRITICAL (xlf_fp_util)
                   yr = yr + ieee_rint(rr)
!SMP$  END CRITICAL (xlf_fp_util)
               end do
!SMP$  END PARALLEL DO

               if (yr /= 360.0) error stop 12
               call ieee_set_status(status_value)
               call set_fpscr_flags(flags(1))
               if ( get_fpscr_flags(flags(1)) .eq. 0 ) error stop 13
            end subroutine sub1

      end program



            logical function fun1()
               use, intrinsic :: ieee_arithmetic
               use, intrinsic :: ieee_exceptions
               use, intrinsic :: xlf_fp_util

               real*4 yr, rr
               type(ieee_round_type), parameter :: rt_nearest = IEEE_NEAREST
               type(ieee_status_type) :: status_value
               integer(fpscr_kind), dimension(5) :: flags

               flags = (/ fp_overflow, fp_div_by_zero, fp_invalid, &
     &               fp_underflow, fp_inexact /)

               call ieee_get_status(status_value)
               call ieee_set_rounding_mode(rt_nearest)

!... Testing CRITICAL section with same name as an INTRINSIC module

               yr = 0.
!SMP$  PARALLEL DO private(rr)
               do i = 1, 100
                   rr = log(real(i))
!SMP$  CRITICAL (ieee_exceptions)
                   yr = yr + ieee_rint(rr)
!SMP$  END CRITICAL (ieee_exceptions)
               end do
!SMP$  END PARALLEL DO

               if (yr /= 360.0) error stop 16
               call ieee_set_status(status_value)
               call set_fpscr_flags(flags(1))
               if ( get_fpscr_flags(flags(1)) .eq. 0 ) error stop 17
               fun1=.true.
            end function fun1

