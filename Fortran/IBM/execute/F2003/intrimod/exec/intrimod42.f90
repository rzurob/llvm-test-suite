! ************************************************************************
! %START
! %MAIN: YES
! %PRECMD: $TR_SRC/intrimod42.presh intrimod42 
! %COMPOPTS: -qhalt=w -qmodule=mangle81
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT: 
! %EXECARGS:
! %POSTCMD:
! %END
!************************************************************************
!************************************************************************
!*
!*  FORTRAN TEST CASE            IBM INTERNAL USE ONLY
!*  Test Case Title  : INTRINSIC/NON_INTRINSIC module nature
!*  Test Case Name   : intrimod42.f
!*  Created By       : Bahram Chehrazy
!*  DATE             : January, 2004
!*  Description      : Testing backward compatibility with XLF8.1.
!*                     Calling module procedures where the modules are 
!*                     pre-compiled with 8.1 and the mainline is compiled
!*                     with Post 8.1 using the Option "-qmodule=mangle81".
!*
!*************************************************************************
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  01/23/04   BC     Initial Version
!*  04/04/08   GM     Defect 299017 - Adjusted Description and COMPOPTS
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890
!

      program intrimod42

         use, non_intrinsic :: ieee_arithmetic
         use, non_intrinsic :: ieee_exceptions
         use, non_intrinsic :: xlf_fp_util
         use my_mod

         real*4 yr
         type(ieee_round_type) :: rtype
         type(ieee_round_type), parameter :: rt_nearest = IEEE_NEAREST
         type(ieee_status_type) :: status_value
         logical :: flag_values(5)
         integer(fpscr_kind), dimension(5) :: flags

         flags = (/ fp_overflow, fp_div_by_zero, fp_invalid, &
     &               fp_underflow, fp_inexact /)


!... sub1 and sub2 must access intrinsic ieee modules
         call sub1()
         call sub2()



!... The non_intrinsic ieee module procedures must be accessed here
         call ieee_get_flag(ieee_all, flag_values)
         do k = 1, 5
            if (flag_values(k) .neqv. .false. ) stop 31
         enddo

         if (ieee_support_datatype(PINF_4) .AND. &
             ieee_support_datatype(NINF_4)) then
            if (ieee_is_finite(PINF_4) .OR. ieee_is_finite(NINF_4)) stop 32
         endif

         if (ieee_support_datatype(PHD_4) .AND.  &
               ieee_support_datatype(PTD_4)) then
             if (ieee_is_finite(PHD_4) .neqv. .true.) stop 33
             if (ieee_is_finite(PTD_4) .neqv. .true.) stop 34
         endif

         call ieee_get_status(status_value)
         call ieee_set_rounding_mode(rt_nearest)
         call ieee_get_rounding_mode(rtype)
         if (rtype.rt /= rt_nearest.rt) stop 35
         yr = ieee_rint(1.1)
         if (yr /= 1.0) stop 36
         call ieee_set_status(status_value)
         call set_fpscr_flags(flags(1))
         call clr_fpscr_flags(flags(5))
         if ( get_fpscr_flags(flags(1)) .eq. 0 ) stop 37
         if ( get_fpscr_flags(flags(5)) .eq. 0 ) stop 38


      end 

