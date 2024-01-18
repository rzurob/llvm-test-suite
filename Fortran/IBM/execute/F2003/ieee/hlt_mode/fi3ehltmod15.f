!*********************************************************************
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : February 15, 2002
!*
!*  PRIMARY FUNCTIONS TESTED   : IEEE_SET_HALTING_MODE,IEEE_GET_HALTING_MODE
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  : -qfloat=nofold -qflttrap=inexact -qsigtrap
!*
!*  KEYWORD(S)                 :
!*
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : Testing IEEE_SET_HALTING_MODE and
!*                               IEEE_GET_HALTING_MODE subroutines for
!*                               REAL*16. Halting on INEXACT exception.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

         program fi3ehltmod15

         use ieee_exceptions
         use constants_for_ieee

         real(16) :: tr1_16
         logical :: actual_flag_value, actual_halting_value
         integer :: caseid
         type(ieee_status_type) :: status_value

!...get floating point status
         call ieee_get_status(status_value)
!...set flags for ieee_all to false
         call ieee_set_flag(ieee_all, .false.)

         caseid = 2

!...IEEE_INEXACT exception occurs:
         if (IEEE_SUPPORT_HALTING(IEEE_INEXACT) .eqv. .false.) then
            call zzrc(caseid)
         endif

!...set halting mode for IEEE_ALL to false
!...execution will continue after any exception will occur
         call ieee_set_halting_mode(IEEE_ALL, .false.)

!...halting on exception IEEE_INEXACT
         call ieee_set_flag(IEEE_INEXACT, .false. )
         call ieee_get_flag(IEEE_INEXACT, actual_flag_value)
         if (actual_flag_value .neqv. .false. ) then
            call zzrc(caseid+1)
         endif

         call ieee_get_halting_mode(IEEE_INEXACT, actual_halting_value)
         if ( actual_halting_value .neqv. .false. ) then
            call zzrc(caseid+2)
         endif

         call ieee_set_halting_mode(IEEE_INEXACT, .true.)
         call ieee_get_halting_mode(IEEE_INEXACT, actual_halting_value)
         if ( actual_halting_value .neqv. .true. ) then
            call zzrc(caseid+3)
         endif

         tr1_16 = zr_16/r3_16
         print *, tr1_16
         call ieee_get_flag(IEEE_INEXACT, actual_flag_value)
         if ( actual_flag_value .neqv. .true. ) then
            call zzrc(caseid+3)
         endif

!...set the floating point status back

         call ieee_set_status(status_value)

         end program


