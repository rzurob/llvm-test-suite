! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : February 11, 2002
!*
!*  PRIMARY FUNCTIONS TESTED   : IEEE_SUPPORT_FLAG
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
	     program fxieee16

        use ieee_exceptions
        ! use ieee_arithmetic
        use constants_for_ieee

        logical(4) :: original(5), log_values(5)

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

        ! test ieee_support_flag

! witnout arg, returen false

	    if (ieee_support_flag(ieee_overflow)) print *, "ieee flag error."
        if (ieee_support_flag(ieee_divide_by_zero)) print *, "ieee flag error."
		if (ieee_support_flag(ieee_invalid)) print *, "ieee flag error."
        if (ieee_support_flag(ieee_underflow)) print *, "ieee flag error."
        if (ieee_support_flag(ieee_inexact)) print *, "ieee flag error."

! for real*4, should return true
        if (ieee_support_flag(ieee_overflow, values(1)) .neqv. .true.) then
            print *, "ieee flag error in real*4."
        endif

        if (ieee_support_flag(ieee_divide_by_zero, values(1)) .neqv. .true.) then
           print *, "ieee flag error in real*4."
        endif

        if (ieee_support_flag(ieee_invalid, values(1)) .neqv. .true.) then
           print *, "ieee flag error in real*4."
        endif

        if (ieee_support_flag(ieee_underflow, values(1)) .neqv. .true.) then
           print *, "ieee flag error in real*4."
        endif

        if (ieee_support_flag(ieee_inexact, values(1)) .neqv. .true.) then
           print *, "ieee flag error in real*4."
        endif

        if (ieee_support_flag(ieee_overflow, values) .neqv. .true.) then
            print *, "ieee flag error in real*4."
        endif

        if (ieee_support_flag(ieee_divide_by_zero, values) .neqv. .true.) then
           print *, "ieee flag error in real*4."
        endif

        if (ieee_support_flag(ieee_invalid, values) .neqv. .true.) then
           print *, "ieee flag error in real*4."
        endif

        if (ieee_support_flag(ieee_underflow, values) .neqv. .true.) then
           print *, "ieee flag error in real*4."
        endif

        if (ieee_support_flag(ieee_inexact, values) .neqv. .true.) then
           print *, "ieee flag error in real*4."
        endif

! for real*8, should return true
        if (ieee_support_flag(ieee_overflow, values_8(1)) .neqv. .true.) then
            print *, "ieee flag error in real*8."
        endif

        if (ieee_support_flag(ieee_divide_by_zero, values_8(1)) .neqv. .true.) then
            print *, "ieee flag error in real*8."
        endif

        if (ieee_support_flag(ieee_invalid, values_8(1)) .neqv. .true.) then
            print *, "ieee flag error in real*8."
        endif

        if (ieee_support_flag(ieee_underflow, values_8(1)) .neqv. .true.) then
           print *, "ieee flag error in real*8."
        endif

        if (ieee_support_flag(ieee_inexact, values_8(1)) .neqv. .true.) then
            print *, "ieee flag error in real*8."
        endif

        if (ieee_support_flag(ieee_overflow, values_8) .neqv. .true.) then
            print *, "ieee flag error in real*8."
        endif

        if (ieee_support_flag(ieee_divide_by_zero, values_8) .neqv. .true.) then
            print *, "ieee flag error in real*8."
        endif

        if (ieee_support_flag(ieee_invalid, values_8) .neqv. .true.) then
            print *, "ieee flag error in real*8."
        endif

        if (ieee_support_flag(ieee_underflow, values_8) .neqv. .true.) then
           print *, "ieee flag error in real*8."
        endif

        if (ieee_support_flag(ieee_inexact, values_8) .neqv. .true.) then
            print *, "ieee flag error in real*8."
        endif

! for real*16, return false
        if (ieee_support_flag(ieee_overflow, values_16(1))) then
           print *, "ieee flag error in real*16."
        endif

        if (ieee_support_flag(ieee_divide_by_zero, values_16(1))) then
            print *, "ieee flag error in real*16."
        endif

        if (ieee_support_flag(ieee_invalid, values_16(1))) then
           print *, "ieee flag error in real*16."
        endif

        if (ieee_support_flag(ieee_underflow, values_16(1))) then
           print *, "ieee flag error in real*16."
        endif

        if (ieee_support_flag(ieee_inexact, values_16(1))) then
            print *, "ieee flag error in real*16."
        endif

        if (ieee_support_flag(ieee_overflow, values_16)) then
           print *, "ieee flag error in real*16."
        endif

        if (ieee_support_flag(ieee_divide_by_zero, values_16)) then
            print *, "ieee flag error in real*16."
        endif

        if (ieee_support_flag(ieee_invalid, values_16)) then
           print *, "ieee flag error in real*16."
        endif

        if (ieee_support_flag(ieee_underflow, values_16)) then
           print *, "ieee flag error in real*16."
        endif

        if (ieee_support_flag(ieee_inexact, values_16)) then
            print *, "ieee flag error in real*16."
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
