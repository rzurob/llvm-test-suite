! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: $TR_SRC/fxieee.presh fclass03
! %COMPOPTS: -qfree=f90 -qfloat=nofold -qstrict
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : F2K IEEE Modules
!*
!*  PROGRAMMER                 : Vasile Radulescu 
!*  DATE                       : February 15, 2002
!*  ORIGIN                     : XL Fortran Development
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : IEEE_CLASS 
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf95
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : Testing IEEE_CLASS for REAL(16)
!*
!*
!234567890123456789012345678901234567890123456789012345678901234567890

        program fclass03
      
        use ieee_arithmetic
        use constants_for_ieee
	
        real(16), dimension(4) :: normal_result	
        real(16), dimension(4) :: arrval 
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
        if (ieee_class(PINF_16) /= ieee_positive_inf) then
           call zzrc(caseid) 
        endif

        if (ieee_class(NINF_16) /= ieee_negative_inf) then
           call zzrc(caseid+1)
        endif  

!test IEEE_CLASS for IEEE_POSITIVE_NORMAL and IEEE_NEGATIVE_NORMAL results 
         
        if (ieee_class(huge(PINF_16)) /= ieee_positive_normal) then
           call zzrc(caseid+2) 
        endif
        if (ieee_class(tiny(PINF_16)) /= ieee_positive_normal) then
           call zzrc(caseid+3) 
        endif
        if (ieee_class(-huge(PINF_16)) /= ieee_negative_normal) then
           call zzrc(caseid+4)
        endif
        if (ieee_class(-tiny(PINF_16)) /= ieee_negative_normal) then
           call zzrc(caseid+5)
        endif


!test IEEE_CLASS for IEEE_POSITIVE_ZERO result

        if (ieee_class(PZERO_16) /= ieee_positive_zero) then
           call zzrc(caseid+6)
        endif

!test IEEE_CLASS for IEEE_POSITIVE_DENORMAL and IEEE_NEGATIVE_DENORMAL

        if (ieee_class(PHD_16) /= ieee_positive_denormal) then
           call zzrc(caseid+8)
        endif

        if (ieee_class(PTD_16) /= ieee_positive_denormal) then
           call zzrc(caseid+9)
        endif

        if (ieee_class(NHD_16) /= ieee_negative_denormal) then
           call zzrc(caseid+10)
        endif

        if (ieee_class(NTD_16) /= ieee_negative_denormal) then
           call zzrc(caseid+11)
        endif

!test IEEE_CLASS with arrays for IEEE_QUIET_NAN and IEEE_SIGNALING_NAN 
        arrval = (/ PNANQ_16, NNANQ_16, PNANS_16, NNANS_16 /)
        actual_results = ieee_class(arrval)

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

!test IEEE_CLASS with NANQ and NANS
!...lowest range values (NANQ)

        if ( ieee_class(pnanq_lowest_16) /= ieee_quiet_nan ) then
           call zzrc(caseid+16)
        endif

        if ( ieee_class(nnanq_lowest_16) /= ieee_quiet_nan ) then
           call zzrc(caseid+17)
        endif

!...highest range values(NANQ)

        if ( ieee_class(pnanq_highest_16) /= ieee_quiet_nan ) then
           call zzrc(caseid+18)
        endif

        if ( ieee_class(nnanq_highest_16) /= ieee_quiet_nan ) then
           call zzrc(caseid+19)
        endif

!...lowest range values(NANS)

        if ( ieee_class(pnans_lowest_16) /= ieee_signaling_nan ) then
           call zzrc(caseid+20)
        endif

        if ( ieee_class(nnans_lowest_16) /= ieee_signaling_nan ) then
           call zzrc(caseid+21)
        endif

!...highest range values(NANS)

        if ( ieee_class(pnans_highest_16) /= ieee_signaling_nan ) then
           call zzrc(caseid+22)
        endif

        if ( ieee_class(nnans_highest_16) /= ieee_signaling_nan ) then
           call zzrc(caseid+23)
        endif

!...test IEEE_CLASS with + and - normal values resulting from operations
        normal_result(1) = PNORMAL1_16/PNORMAL2_16

        if (ieee_class(normal_result(1)) /= ieee_positive_normal ) then
           call zzrc(caseid+24)
        endif

        normal_result(2) = NNORMAL1_16/PNORMAL2_16

        if (ieee_class(normal_result(2)) /= ieee_negative_normal ) then
           call zzrc(caseid+25)
        endif

        normal_result(3) = NNORMAL1_16/NNORMAL2_16

        if (ieee_class(normal_result(3)) /= ieee_positive_normal ) then
           call zzrc(caseid+26)
        endif

        normal_result(4) = PNORMAL1_16/NNORMAL2_16

        if (ieee_class(normal_result(4)) /= ieee_negative_normal ) then
           call zzrc(caseid+27)
        endif

!...Check that no flags were turned on by IEEE_CLASS 
        call ieee_get_flag(ieee_all, actual_flag_values)
        do k = 1,5
           if (actual_flag_values(k) .neqv. .false. ) then
              call zzrc(caseid+28)
           endif
        enddo

        end
