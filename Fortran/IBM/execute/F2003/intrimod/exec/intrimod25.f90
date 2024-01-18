! ************************************************************************
!************************************************************************
!*
!*  Created By       : Bahram Chehrazy
!*  DATE             : January, 2004
!*  Description      : Use INTRINSIC module with same name as binding label
!*                     for variables.
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
          use, intrinsic :: ieee_arithmetic
          use, intrinsic :: xlf_fp_util
          use, intrinsic :: iso_c_binding

          real*4 yr
          type(ieee_round_type), parameter :: rt_nearest = IEEE_NEAREST
          type(ieee_status_type) :: status_value
          logical :: flag_values(5)

!.... Binding labels same as intrinsic modules.
          BIND(C, name="ieee_arithmetic") :: flag_values
          BIND(C, name="iso_c_binding") :: yr

       end module


       program intrimod25

  	 use, intrinsic :: ieee_arithmetic
         use, intrinsic :: xlf_fp_util
         use, intrinsic :: ISO_C_BINDING
         use mod1

         interface
	     integer function c_fun1()
             end function c_fun1
	     integer function c_fun2()
             end function c_fun2
         end interface

         call ieee_get_flag(ieee_all, flag_values)
         do k = 1, 5
            if (flag_values(k) .neqv. .false. ) stop 10
         enddo

         if( c_fun1() /= 0 ) stop 12

         call ieee_get_status(status_value)
         call ieee_set_rounding_mode(rt_nearest)
         yr = ieee_rint(1.1)
         if (yr /= 1.0) stop 16

         if (c_fun2() /= 0) stop 17

         call ieee_set_status(status_value)

       end program
