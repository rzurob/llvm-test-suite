!*********************************************************************
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : February 15, 2002
!*
!*  PRIMARY FUNCTIONS TESTED   : IEEE_SET_HALTING_MODE,IEEE_GET_HALTING_MODE
!*
!*  SECONDARY FUNCTIONS TESTED : IEEE_SET_STATUS,IEEE_GET_STATUS
!*                               IEEE_SET_FLAG,IEEE_GET_FLAG
!*
!*  REQUIRED COMPILER OPTIONS  : -qfloat=nofold -qflttrap=invalid -qsigtrap
!*
!*  KEYWORD(S)                 :
!*
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : Testing IEEE_SET_HALTING_MODE and
!*                               IEEE_GET_HALTING_MODE subroutines for
!*                               REAL*16. Halting on INVALID exception.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

         program fi3ehltmod13

         use ieee_arithmetic
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

!...IEEE_INVALID exception occurs:
         if (IEEE_SUPPORT_HALTING(IEEE_INVALID) .eqv. .false.) then
            call zzrc(caseid)
         endif

!...set halting mode for IEEE_ALL to false
!...execution will continue after any exception will occur
         call ieee_set_halting_mode(IEEE_ALL, .false.)

!...halting on exception IEEE_INVALID
         call ieee_set_flag(IEEE_INVALID, .false. )
         call ieee_get_flag(IEEE_INVALID, actual_flag_value)
         if (actual_flag_value .neqv. .false. ) then
            call zzrc(caseid+1)
         endif

         call ieee_get_halting_mode(IEEE_INVALID, actual_halting_value)
         if ( actual_halting_value .neqv. .false. ) then
            call zzrc(caseid+2)
         endif

         call ieee_set_halting_mode(IEEE_INVALID, .true.)
         call ieee_get_halting_mode(IEEE_INVALID, actual_halting_value)
         if ( actual_halting_value .neqv. .true. ) then
            call zzrc(caseid+3)
         endif

         tr_16 = r1_16/r1_16
         print *, tr_16
         call ieee_get_flag(IEEE_INVALID, actual_flag_value)
         if ( actual_flag_value .neqv. .true.) then
            call zzrc(caseid+4)
         endif
         if (ieee_is_nan(r3_16) .neqv. .true. ) then
            call zzrc(caseid+5)
         endif

!...set the floating point status back

         call ieee_set_status(status_value)

         end program

