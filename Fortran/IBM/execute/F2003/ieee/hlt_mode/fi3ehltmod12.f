!*********************************************************************
! %START
! %MAIN: YES
! %PRECMD: $TR_SRC/fxi3e.presh fi3ehltmod12
! %COMPOPTS: -qfloat=nofold -qflttrap=zerodivide -qsigtrap -qfree=f90 -qstrict
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
!*  PRIMARY FUNCTIONS TESTED   : IEEE_SET_HALTING_MODE,IEEE_GET_HALTING_MODE
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  : -qfloat=nofold -qflttrap=zerodivide -qsigtrap
!*
!*  KEYWORD(S)                 :
!*
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : Testing IEEE_SET_HALTING_MODE and
!*                               IEEE_GET_HALTING_MODE subroutines for
!*                               REAL*16. Halting on IEEE_DIVIDE_BY_ZERO.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

         program fi3ehltmod12

         use ieee_exceptions
         use constants_for_ieee

         real(16) :: tr_16
         logical :: actual_flag_value, actual_halting_value
         integer :: caseid
         type(ieee_status_type) :: status_value

!...get floating point status
         call ieee_get_status(status_value)
!...set flags for ieee_all to false
         call ieee_set_flag(ieee_all, .false.)

         caseid = 2

!...check if the processor supports halting process when
!...IEEE_DIVIDE_BY_ZERO exception occurs:
         if (IEEE_SUPPORT_HALTING(IEEE_DIVIDE_BY_ZERO) .eqv. .false.) then
            call zzrc(caseid)
         endif

!...set halting mode for IEEE_ALL to false
!...execution will continue after any exception will occur
         call ieee_set_halting_mode(IEEE_ALL, .false.)

!...halting on exception IEEE_DIVIDE_BY_ZERO
!...for positive value (the result should be INF)
         call ieee_get_flag(IEEE_DIVIDE_BY_ZERO, actual_flag_value)
         if (actual_flag_value .neqv. .false. ) then
            call zzrc(caseid+1)
         endif

         call ieee_get_halting_mode(IEEE_DIVIDE_BY_ZERO, actual_halting_value)
         if ( actual_halting_value .neqv. .false. ) then
            call zzrc(caseid+2)
         endif

         call ieee_set_halting_mode(IEEE_DIVIDE_BY_ZERO, .true.)
         call ieee_get_halting_mode(IEEE_DIVIDE_BY_ZERO, actual_halting_value)
         if ( actual_halting_value .neqv. .true. ) then
            call zzrc(caseid+3)
         endif

         tr_16 = r2_16/r1_16
         print *, tr_16
         call ieee_get_flag(IEEE_DIVIDE_BY_ZERO, actual_flag_value)
         if ( actual_flag_value .neqv. .true. ) then
            call zzrc(caseid+4)
         endif
         if ( tr_16 /= PINF_16) then
            call zzrc(caseid+5)
         endif

!...set the floating point status back

         call ieee_set_status(status_value)

         end program


