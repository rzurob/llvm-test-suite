! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : February 11, 2002
!*
!*  PRIMARY FUNCTIONS TESTED   : IEEE_SUPPORT_STANDARD
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
	     program fxieee20

        use ieee_arithmetic
        use constants_for_ieee

        logical(4) :: original(5), flag_values(5)

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

! Ensure all exception flags are quiet
        call ieee_set_flag(ieee_all,.false.)

        if (ieee_support_standard()) print *, "ieee support standard error."

        if (ieee_support_standard(values(1)) .eqv. .false.) then
		     print *, "ieee support standard error in real*4."
		  endif

        if (ieee_support_standard(values) .eqv. .false.) then
           print *, "ieee support standard error in real*4."
		  endif

        if (ieee_support_standard(values_8(1)) .eqv. .false.) then
		     print *, "ieee support standard error in real*8."
		  endif

        if (ieee_support_standard(values_8) .eqv. .false.) then
           print *, "ieee support standard error in real*8."
		  endif

        if (ieee_support_standard(values_16(1))) then
           print *, "ieee support standard error in real*16."
        endif

        if (ieee_support_standard(values_16)) then
           print *, "ieee support standard error in real*16."
        endif

        call ieee_get_flag(ieee_all, flag_values)
		do i = 1, 5
		   if (flag_values(i) .eqv. .true.) then
		      print *, "Error, flag ", i, " was set!"
		   endif
		enddo

! set flags back to original
        call ieee_set_flag(ieee_all, original)

        end
