! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : January, 2004
!*
!*  PRIMARY FUNCTIONS TESTED   : -qfloat=nans
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : Re-using /tstdev/ieee/unit/fxieee22.f
!*                               to test INTRINSIC module nature.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
	     program fxieee22

        use,intrinsic :: ieee_arithmetic
        use,non_intrinsic :: constants_for_ieee

        real*4, dimension(10) :: values
        real*8, dimension(10) :: values_8
		real*16, dimension(10) :: values_16

        logical, dimension(5) :: flag_values
        integer :: i

        type(ieee_class_type), parameter :: ctypes(10) = &
     &  (/                      &
     &  ieee_signaling_nan,     &
     &  ieee_quiet_nan,         &
     &  ieee_negative_inf,      &
     &  ieee_positive_inf,      &
     &  ieee_negative_normal,   &
     &  ieee_positive_normal,   &
     &  ieee_negative_denormal, &
     &  ieee_positive_denormal, &
     &  ieee_negative_zero,     &
     &  ieee_positive_zero      &
     &   /)

!       ieee_value should not set any flags.  Clear all flags and
!       check at the end that all flags are clear.
        call ieee_set_flag(ieee_all,.false.)

!       Test real*4
        values = ieee_value(values, ctypes)


! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1, 5
           if (flag_values(i) .neqv. .false.) then
               print *, "ieee_class failed: An exception flag (",i,") was set."
           endif
        enddo

        end program
