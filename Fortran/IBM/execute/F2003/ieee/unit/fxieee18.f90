! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : February 11, 2002
!*
!*  PRIMARY FUNCTIONS TESTED   : IEEE_SUPPORT_ROUNDING
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!234567890123456789012345678901234567890123456789012345678901234567890
	    program fxieee18

        use ieee_exceptions
        use ieee_arithmetic
        use constants_for_ieee

        logical(4) :: original(5), log_values(5), flag

        integer :: i

        real*4, parameter, dimension(2) :: values = &
     &       (/             &
     &       huge(1.0),  & ! Positive Normal
     &       tiny(1.0)   & ! Positive Normal
     &       /)

    	  real*8, parameter, dimension(2) :: values_8 = &
     &       (/             &
     &       huge(1.0_8),  & ! Positive Normal
     &       tiny(1.0_8)   & ! Positive Normal
     &       /)

        real*16, parameter, dimension(2) :: values_16 = &
     &       (/              &
     &       huge(1.0_16),  & ! Positive Normal
     &       tiny(1.0_16)   & ! Positive Normal
     &       /)


! get original flags
        call ieee_get_flag(ieee_all, original)

! witnout arg, returen false

        if (ieee_support_rounding(ieee_nearest)) print *, "support rounding error."
        if (ieee_support_rounding(ieee_to_zero)) print *, "support rounding error."
		if (ieee_support_rounding(ieee_up)) print *, "support rounding error."
        if (ieee_support_rounding(ieee_down)) print *, "support rounding error."
        if (ieee_support_rounding(ieee_other)) print *, "support rounding error."

! test real*4
        if (ieee_support_rounding(ieee_nearest, values(1)) .eqv. .false.) then
           print *, "support rounding error in real*4."
        endif

        if (ieee_support_rounding(ieee_nearest, values) .eqv. .false.) then
           print *, "support rounding error in real*4."
        endif

        if (ieee_support_rounding(ieee_to_zero, values(1)) .eqv. .false.) then
		     print *, "support rounding error in real*4."
		  endif

        if (ieee_support_rounding(ieee_to_zero, values) .eqv. .false.) then
		     print *, "support rounding error in real*4."
		  endif

        if (ieee_support_rounding(ieee_up, values(1)) .eqv. .false.) then
		     print *, "support rounding error in real*4."
        endif

		  if (ieee_support_rounding(ieee_up, values) .eqv. .false.) then
		     print *, "support rounding error in real*4."
        endif

        if (ieee_support_rounding(ieee_down, values(1)) .eqv. .false.) then
		     print *, "support rounding error in real*4."
        endif

		  if (ieee_support_rounding(ieee_down, values) .eqv. .false.) then
		     print *, "support rounding error in real*4."
        endif

        if (ieee_support_rounding(ieee_other, values(1)) ) then
		     print *, "support rounding error in real*4."
        endif

		  if (ieee_support_rounding(ieee_other, values) ) then
		     print *, "support rounding error in real*4."
        endif

! test real*8
        if (ieee_support_rounding(ieee_nearest, values_8(1)) .eqv. .false.) then
           print *, "support rounding error in real*8."
        endif

        if (ieee_support_rounding(ieee_nearest, values_8) .eqv. .false.) then
           print *, "support rounding error in real*8."
        endif

        if (ieee_support_rounding(ieee_to_zero, values_8(1)) .eqv. .false.) then
		     print *, "support rounding error in real*8."
		  endif

        if (ieee_support_rounding(ieee_to_zero, values_8) .eqv. .false.) then
		     print *, "support rounding error in real*8."
		  endif

        if (ieee_support_rounding(ieee_up, values_8(1)) .eqv. .false.) then
		     print *, "support rounding error in real*8."
        endif

		  if (ieee_support_rounding(ieee_up, values_8) .eqv. .false.) then
		     print *, "support rounding error in real*8."
        endif

        if (ieee_support_rounding(ieee_down, values_8(1)) .eqv. .false.) then
		     print *, "support rounding error in real*8."
        endif

		  if (ieee_support_rounding(ieee_down, values_8) .eqv. .false.) then
		     print *, "support rounding error in real*8."
        endif

        if (ieee_support_rounding(ieee_other, values_8(1)) ) then
		     print *, "support rounding error in real*8."
        endif

		if (ieee_support_rounding(ieee_other, values_8) ) then
		     print *, "support rounding error in real*8."
        endif

! test real*16
        if (ieee_support_rounding(ieee_nearest, values_16(1))) then
           print *, "support rounding error in real*16 for nearest."
        endif

        if (ieee_support_rounding(ieee_nearest, values_16)) then
           print *, "support rounding error in real*16 for nearest."
        endif

        if (ieee_support_rounding(ieee_to_zero, values_16(1))) then
		     print *, "support rounding error in real*16."
		  endif

        if (ieee_support_rounding(ieee_to_zero, values_16) ) then
		     print *, "support rounding error in real*16."
		  endif

        if (ieee_support_rounding(ieee_up, values_16(1)) ) then
		     print *, "support rounding error in real*16."
        endif

		  if (ieee_support_rounding(ieee_up, values_16) ) then
		     print *, "support rounding error in real*16."
        endif

        if (ieee_support_rounding(ieee_down, values_16(1)) ) then
		     print *, "support rounding error in real*16."
        endif

		  if (ieee_support_rounding(ieee_down, values_16) ) then
		     print *, "support rounding error in real*16."
        endif

        if (ieee_support_rounding(ieee_other, values_16(1)) ) then
		     print *, "support rounding error in real*16."
        endif

		  if (ieee_support_rounding(ieee_other, values_16) ) then
		     print *, "support rounding error in real*16."
        endif

! ieee_flag_type should be the same as before
        call ieee_get_flag(ieee_all, log_values)
        do i = 1, 5
           if (log_values(i) .neqv. original(i)) then
              print *, "Error, flag ", i, " was set!"
           endif
        enddo

! set flags back to original
        call ieee_set_flag(ieee_all, original)

        end
