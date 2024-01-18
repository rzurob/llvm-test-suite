! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : February 4, 2002
!*
!*  PRIMARY FUNCTIONS TESTED   : IEEE_CLASS
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
        program fxieee01

        use ieee_arithmetic
        use constants_for_ieee

        real(4), dimension(2) :: args4
        real(8), dimension(2) :: args8
		real(16), dimension(2) :: args16
        type(ieee_class_type), dimension(2) :: results
        logical, dimension(5) :: flag_values
        integer :: i

        ! ieee_class should not set any flags.  Clear all flags and
        ! check at the end that all flags are clear.
        call ieee_set_flag(ieee_all,.false.)

        ! Test operation for real(4)'s

        if (ieee_class(PINF_4) /= ieee_positive_inf) then
            print *, "ieee_class(PINF_4) failed."
        endif

        if (ieee_class(huge(PINF_4)) /= ieee_positive_normal) then
            print *, "ieee_class(HUGE) failed."
        endif

        if (ieee_class(tiny(PINF_4)) /= ieee_positive_normal) then
            print *, "ieee_class(TINY) failed."
        endif

        if (ieee_class(PHD_4) /= ieee_positive_denormal) then
            print *, "ieee_class(PHD_4) failed."
        endif

        if (ieee_class(PTD_4) /= ieee_positive_denormal) then
            print *, "ieee_class(PTD_4) failed."
        endif

        if (ieee_class(PZERO_4) /= ieee_positive_zero) then
            print *, "ieee_class(PZERO_4) failed."
        endif

        if (ieee_class(NZERO_4) /= ieee_negative_zero) then
            print *, "ieee_class(NZERO_4) failed."
        endif

        if (ieee_class(NTD_4) /= ieee_negative_denormal) then
            print *, "ieee_class(NTD_4) failed."
        endif

        if (ieee_class(NHD_4) /= ieee_negative_denormal) then
            print *, "ieee_class(NHD_4) failed."
        endif

        if (ieee_class(-tiny(PINF_4)) /= ieee_negative_normal) then
            print *, "ieee_class(-TINY) failed."
        endif

        if (ieee_class(-huge(PINF_4)) /= ieee_negative_normal) then
            print *, "ieee_class(-HUGE) failed."
        endif

        if (ieee_class(NINF_4) /= ieee_negative_inf) then
            print *, "ieee_class(NINF_4) failed."
        endif

        args4 = (/ NNANQ_4, NNANS_4 /)
        results = ieee_class(args4)
        if (results(1) /= ieee_quiet_nan) then
            print *, "ieee_class(array) failed for NNANQ_4."
        endif
        if (results(2) /= ieee_signaling_nan) then
            print *, "ieee_class(array) failed for NNANS_4."
        endif


        ! Test Operation for real(8)

        if (ieee_class(PINF_8) /= ieee_positive_inf) then
            print *, "ieee_class(PINF_8) failed."
        endif

        if (ieee_class(huge(PINF_8)) /= ieee_positive_normal) then
            print *, "ieee_class(HUGE) failed."
        endif

        if (ieee_class(tiny(PINF_8)) /= ieee_positive_normal) then
            print *, "ieee_class(TINY) failed."
        endif

        if (ieee_class(PHD_8) /= ieee_positive_denormal) then
            print *, "ieee_class(PHD_8) failed."
        endif

        if (ieee_class(PTD_8) /= ieee_positive_denormal) then
            print *, "ieee_class(PTD_8) failed."
        endif

        if (ieee_class(PZERO_8) /= ieee_positive_zero) then
            print *, "ieee_class(PZERO_8) failed."
        endif

        if (ieee_class(NZERO_8) /= ieee_negative_zero) then
            print *, "ieee_class(NZERO_8) failed."
        endif

        if (ieee_class(NTD_8) /= ieee_negative_denormal) then
            print *, "ieee_class(NTD_8) failed."
        endif

        if (ieee_class(NHD_8) /= ieee_negative_denormal) then
            print *, "ieee_class(NHD_8) failed."
        endif

        if (ieee_class(-tiny(PINF_8)) /= ieee_negative_normal) then
            print *, "ieee_class(-TINY) failed."
        endif

        if (ieee_class(-huge(PINF_8)) /= ieee_negative_normal) then
            print *, "ieee_class(-HUGE) failed."
        endif

        if (ieee_class(NINF_8) /= ieee_negative_inf) then
            print *, "ieee_class(NINF_8) failed."
        endif

        args8 = (/NNANQ_8,NNANS_8/)
        results = ieee_class(args8)
        if (results(1) /= ieee_quiet_nan) then
            print *, "ieee_class(array) failed for NNANQ_8."
        endif
        if (results(2) /= ieee_signaling_nan) then
            print *, "ieee_class(array) failed for NNANS_8."
        endif

 ! Test Operation for real(16)

        if (ieee_class(PINF_16) /= ieee_positive_inf) then
            print *, "ieee_class(PINF_16) failed."
        endif

        if (ieee_class(huge(1.0_16)) /= ieee_positive_normal) then
            print *, "ieee_class(HUGE in 16) failed."
        endif

        if (ieee_class(tiny(1.0_16)) /= ieee_positive_normal) then
            print *, "ieee_class(TINY in 16) failed."
        endif

!        if (ieee_class(PHD_16) /= ieee_positive_denormal) then
!            print *, "ieee_class(PHD_16) failed."
!        endif

!        if (ieee_class(PTD_16) /= ieee_positive_denormal) then
!            print *, "ieee_class(PTD_16) failed."
!        endif

        if (ieee_class(PZERO_16) /= ieee_positive_zero) then
            print *, "ieee_class(PZERO_16) failed."
        endif

        if (ieee_class(PZERO2_16) /= ieee_negative_zero) then
            print *, "ieee_class(PZERO2_16) failed."
        endif

!        if (ieee_class(NTD_8) /= ieee_negative_denormal) then
!            print *, "ieee_class(NTD_8) failed."
!        endif

!        if (ieee_class(NHD_8) /= ieee_negative_denormal) then
!            print *, "ieee_class(NHD_8) failed."
!        endif

        if (ieee_class(-tiny(1.0_16)) /= ieee_negative_normal) then
            print *, "ieee_class(-TINY in 16) failed."
        endif

        if (ieee_class(-huge(1.0_16)) /= ieee_negative_normal) then
            print *, "ieee_class(-HUGE in 16) failed."
        endif

        if (ieee_class(NINF_16) /= ieee_negative_inf) then
            print *, "ieee_class(NINF_16) failed."
        endif

        args16 = (/NNANQ_16,NNANS_16/)
        results = ieee_class(args16)
        if (results(1) /= ieee_quiet_nan) then
            print *, "ieee_class(array) failed for NNANQ_16."
        endif
        if (results(2) /= ieee_signaling_nan) then
            print *, "ieee_class(array) failed for NNANS_16."
        endif

        ! Now check that no flags were turned on.
        call ieee_get_flag(ieee_all,flag_values)
        do i = 1,5
            if (flag_values(i) .neqv. .false.) then
                print *, "ieee_class failed: An exception flag (",i,") was set."
            endif
        enddo

        end

