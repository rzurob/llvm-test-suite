! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : February 15, 2002
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
!*  DESCRIPTION                : Testing IEEE_CLASS for REAL(8)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

        program fclass02

        use ieee_arithmetic
        use constants_for_ieee

        real(8), dimension(4) :: normal_result
        real(8), dimension(4) :: arrval
        type(ieee_class_type), dimension(4) :: actual_results
        logical, dimension(5) :: actual_flag_values
        integer :: k, caseid

!...Check that all flags are false
!... if not set to .false.
        call ieee_set_flag(ieee_all,.false.)
        do k =1, 5
           if (actual_flag_values(k) .neqv. .false. ) then
              call ieee_set_flag(ieee_all(k), .false. )
           endif
        enddo

        caseid = 1

!test IEEE_CLASS for IEEE_POSITIVE_INF and IEEE_NEGATIVE_INF results
        if (ieee_support_inf(PINF_8)) then
           if (ieee_class(PINF_8) /= ieee_positive_inf) then
              call zzrc(caseid)
           endif
        endif

        if (ieee_support_inf(NINF_8)) then
           if (ieee_class(NINF_8) /= ieee_negative_inf) then
              call zzrc(caseid+1)
           endif
        endif

!test IEEE_CLASS for IEEE_POSITIVE_NORMAL and IEEE_NEGATIVE_NORMAL results
        if (ieee_support_inf(PINF_8)) then
           if (ieee_class(huge(PINF_8)) /= ieee_positive_normal) then
              call zzrc(caseid+2)
           endif
           if (ieee_class(tiny(PINF_8)) /= ieee_positive_normal) then
              call zzrc(caseid+3)
           endif
           if (ieee_class(-huge(PINF_8)) /= ieee_negative_normal) then
              call zzrc(caseid+4)
           endif
           if (ieee_class(-tiny(PINF_8)) /= ieee_negative_normal) then
              call zzrc(caseid+5)
           endif
        endif

!test IEEE_CLASS for IEEE_POSITIVE_ZERO and IEEE_NEGATIVE_ZERO results
        if (ieee_support_inf(PZERO_8)) then
           if (ieee_class(PZERO_8) /= ieee_positive_zero) then
              call zzrc(caseid+6)
           endif
        endif
        if (ieee_support_inf(NZERO_8)) then
           if (ieee_class(NZERO_8) /= ieee_negative_zero) then
              call zzrc(caseid+7)
           endif
         endif

!test IEEE_CLASS for IEEE_POSITIVE_DENORMAL and IEEE_NEGATIVE_DENORMAL
        if (ieee_support_denormal(PHD_8)) then
           if (ieee_class(PHD_8) /= ieee_positive_denormal) then
              call zzrc(caseid+8)
           endif
        endif
        if (ieee_support_denormal(PTD_8)) then
           if (ieee_class(PTD_8) /= ieee_positive_denormal) then
              call zzrc(caseid+9)
           endif
        endif

        if (ieee_support_denormal(NHD_8)) then
           if (ieee_class(NHD_8) /= ieee_negative_denormal) then
              call zzrc(caseid+10)
           endif
        endif
        if (ieee_support_denormal(NTD_8)) then
           if (ieee_class(NTD_8) /= ieee_negative_denormal) then
              call zzrc(caseid+11)
           endif
        endif

!test IEEE_CLASS with arrays for IEEE_QUIET_NAN and IEEE_SIGNALING_NAN
        arrval = (/ PNANQ_8, NNANQ_8, PNANS_8, NNANS_8 /)
        actual_results = ieee_class(arrval)

        if (ieee_support_nan(arrval)) then
           if (actual_results(1) /= ieee_quiet_nan) then
              call zzrc(caseid+12)
           endif
           if (actual_results(2) /= ieee_quiet_nan) then
              call zzrc(caseid+13)
           endif
           if (actual_results(3) /= ieee_signaling_nan) then
              call zzrc(caseid+14)
           endif
           if (actual_results(4) /= ieee_signaling_nan) then
              call zzrc(caseid+15)
           endif
        endif

!test IEEE_CLASS with NANQ and NANS
!...lowest range values (NANQ)
        if (ieee_support_datatype(pnanq_lowest_8)) then
           if ( ieee_class(pnanq_lowest_8) /= ieee_quiet_nan ) then
              call zzrc(caseid+16)
           endif
        endif

        if (ieee_support_datatype(nnanq_lowest_8)) then
           if ( ieee_class(nnanq_lowest_8) /= ieee_quiet_nan ) then
              call zzrc(caseid+17)
           endif
        endif

!...highest range values(NANQ)
        if (ieee_support_datatype(pnanq_highest_8)) then
           if ( ieee_class(pnanq_highest_8) /= ieee_quiet_nan ) then
              call zzrc(caseid+18)
           endif
        endif

        if (ieee_support_datatype(nnanq_highest_8)) then
           if ( ieee_class(nnanq_highest_8) /= ieee_quiet_nan ) then
              call zzrc(caseid+19)
           endif
        endif

!...lowest range values(NANS)
        if (ieee_support_datatype(pnans_lowest_8)) then
           if ( ieee_class(pnans_lowest_8) /= ieee_signaling_nan ) then
              call zzrc(caseid+20)
           endif
        endif

        if (ieee_support_datatype(nnans_lowest_8)) then
           if ( ieee_class(nnans_lowest_8) /= ieee_signaling_nan ) then
              call zzrc(caseid+21)
           endif
        endif

!...highest range values(NANS)
        if (ieee_support_datatype(pnans_highest_8)) then
           if ( ieee_class(pnans_highest_8) /= ieee_signaling_nan ) then
              call zzrc(caseid+22)
           endif
        endif

        if (ieee_support_datatype(nnans_highest_8)) then
           if ( ieee_class(nnans_highest_8) /= ieee_signaling_nan ) then
              call zzrc(caseid+23)
           endif
        endif

!...test IEEE_CLASS with + and - normal values resulting from operations
        normal_result(1) = PNORMAL1_8/PNORMAL2_8
        if (ieee_support_datatype(normal_result)) then
           if (ieee_class(normal_result(1)) /= ieee_positive_normal ) then
              call zzrc(caseid+24)
           endif
        endif

        normal_result(2) = NNORMAL1_8/PNORMAL2_8
        if (ieee_support_datatype(normal_result)) then
           if (ieee_class(normal_result(2)) /= ieee_negative_normal ) then
              call zzrc(caseid+25)
           endif
        endif

        normal_result(3) = NNORMAL1_8/NNORMAL2_8
        if (ieee_support_datatype(normal_result)) then
           if (ieee_class(normal_result(3)) /= ieee_positive_normal ) then
              call zzrc(caseid+26)
           endif
        endif

        normal_result(4) = PNORMAL1_8/NNORMAL2_8
        if (ieee_support_datatype(normal_result)) then
           if (ieee_class(normal_result(4)) /= ieee_negative_normal ) then
              call zzrc(caseid+27)
           endif
        endif

!...Check that no flags were turned on by IEEE_CLASS
        call ieee_get_flag(ieee_all, actual_flag_values)
        do k = 1,5
           if (actual_flag_values(k) .neqv. .false. ) then
              call zzrc(caseid+28)
           endif
        enddo

        end