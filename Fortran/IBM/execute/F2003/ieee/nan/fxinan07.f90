! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: $TR_SRC/fxi3e2.presh fxinan07
! %COMPOPTS:  -qfloat=nans:nofold -qautodbl=dbl4 -qfree=f90 -qstrict
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : February 15, 2002
!*
!*  PRIMARY FUNCTIONS TESTED   : IEEE_IS_NAN
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  : -qfloat=nans:nofold -qautodbl=dbl4
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : Testing the elemental function IEEE_IS_NAN
!*                               for REAL(8) which is resulting from conversion
!*                               of REAL(4) using -qautodbl=dbl4 compiler option
!*
!234567890123456789012345678901234567890123456789012345678901234567890

        program fxinan07

        use ieee_arithmetic
        use constants_for_ieee

        real*4 plus_nanq, minus_nanq, plus_nans, minus_nans
        real*4 plus_inf, minus_inf, large
        real*4, dimension(2) :: nan_result
        real*4, dimension(4) :: arrval
        logical :: actual_results(4), expected_results(4), actual_flag_values(5)
        type(ieee_status_type) :: status_value
        integer :: caseid, k
        data plus_inf /z'7FF0000000000000'/
        data minus_inf /z'FFF0000000000000'/

        caseid = 1
!...set flags for ieee_all to false
        call ieee_set_flag(ieee_all,.false.)

        if (ieee_support_nan(PINF_8)) then
           if (ieee_is_nan(PINF_8) .AND. ieee_is_nan(NINF_8)) then
             call zzrc(caseid)
           endif
        endif

        if (ieee_is_nan(PHD_8) .AND. ieee_is_nan(NHD_8)) then
           call zzrc(caseid+1)
        endif

        if (ieee_is_nan(PTD_8) .AND. ieee_is_nan(NTD_8)) then
           call zzrc(caseid+2)
        endif

        if (ieee_is_nan(PZERO_8) .AND. ieee_is_nan(NZERO_8)) then
           call zzrc(caseid+3)
        endif

        arrval = (/ PNANQ_8, PNANS_8, NNANQ_8, NNANS_8 /)
        expected_results = (/.true., .true., .true., .true./)
        if (ieee_support_nan(arrval) ) then
           actual_results = ieee_is_nan(arrval)

           if (actual_results(1) .neqv. expected_results(1)) then
             call zzrc(caseid+4)
           endif

           if (actual_results(2) .neqv. expected_results(2)) then
             call zzrc(caseid+5)
           endif

           if (actual_results(3) .neqv. expected_results(3)) then
             call zzrc(caseid+6)
           endif

           if (actual_results(4) .neqv. expected_results(4)) then
             call zzrc(caseid+7)
           endif
        endif

!...test the range values for NANQ
!...lowest range values
        plus_nanq = z'7FF8000000000000'
        if ( ieee_is_nan(plus_nanq) .neqv. .true. ) then
           call zzrc(caseid+8)
        endif

        minus_nanq = z'FFF8000000000000'
        if ( ieee_is_nan(minus_nanq) .neqv. .true. ) then
           call zzrc(caseid+9)
        endif

!...highest range values
        plus_nanq = z'7FFFFFFFFFFFFFFF'
        if ( ieee_is_nan(plus_nanq) .neqv. .true. ) then
           call zzrc(caseid+10)
        endif

        minus_nanq = z'FFFFFFFFFFFFFFFF'
        if ( ieee_is_nan(minus_nanq) .neqv. .true. ) then
           call zzrc(caseid+11)
        endif

!...test the range values for NANS
!...lowest range values
        plus_nans = z'7FF0000000000001'

        if ( ieee_is_nan(plus_nans) .neqv. .true. ) then
           call zzrc(caseid+12)
        endif

        minus_nans = z'FFF0000000000001'
        if ( ieee_is_nan(minus_nans) .neqv. .true. ) then
           call zzrc(caseid+13)
        endif

!...highest range values
        plus_nans = z'7FF7FFFFFFFFFFFF'
        if ( ieee_is_nan(plus_nans) .neqv. .true. ) then
           call zzrc(caseid+14)
        endif

        minus_nans = z'FFF7FFFFFFFFFFFF'
        if ( ieee_is_nan(minus_nans) .neqv. .true. ) then
           call zzrc(caseid+15)
        endif


!...test NAN values resulting from invalid operations

        nan_result(1) = plus_inf - plus_inf
        if ( ieee_is_nan(nan_result(1)) .neqv. .true. ) then
           call zzrc(caseid+16)
        endif

        large = 10.0_8 ** 200.0_8
        nan_result(2) = SQRT(-large)
        if ( ieee_is_nan(nan_result(2)) .neqv. .true. ) then
           call zzrc(caseid+17)
        endif

        end program
