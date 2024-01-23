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
!*  REQUIRED COMPILER OPTIONS  : -qfloat=nofold -qflttrap=underflow -qsigtrap
!*
!*  KEYWORD(S)                 :
!*
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : Testing IEEE_SET_HALTING_MODE and
!*                               IEEE_GET_HALTING_MODE subroutines for
!*                               REAL*8. Halting on IEEE_UNDERFLOW exception.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

         include 'ieeeconsts.ft'

         program fi3ehltmod09

         use ieee_exceptions
         use constants_for_ieee

         real*8 :: tr1_8, tr2_8
         logical :: actual_flag_value, actual_halting_value
         integer :: caseid
         type(ieee_status_type) :: status_value

!...get floating point status
         call ieee_get_status(status_value)
!...set flags for ieee_all to false
         call ieee_set_flag(ieee_all, .false.)

         caseid = 2

!...check if the processor supports halting process when
!...IEEE_UNDERFLOW exception occurs:
         if (IEEE_SUPPORT_HALTING(IEEE_UNDERFLOW) .eqv. .false.) then
            call zzrc(caseid)
         endif
!...set halting mode for IEEE_ALL to false
!...execution will continue after any exception will occur
         call ieee_set_halting_mode(IEEE_ALL, .false.)

!...halting on exception IEEE_UNDERFLOW
         call ieee_set_flag(IEEE_UNDERFLOW, .false. )
         call ieee_get_flag(IEEE_UNDERFLOW, actual_flag_value)
         if (actual_flag_value .neqv. .false. ) then
            call zzrc(caseid+1)
         endif

         call ieee_set_halting_mode(IEEE_UNDERFLOW, .true.)
         tr1_8 = tiny(r2_8)
         tr2_8 = tr1_8/r3_8
         print *, tr2_8
         call ieee_get_flag(IEEE_UNDERFLOW, actual_flag_value)
         if ( actual_flag_value .neqv. .true. ) then
            call zzrc(caseid+2)
         endif
         if ( tr2_8 == PTD_8 ) then
            call zzrc(caseid+3)
         endif


!...set the floating point status back

         call ieee_set_status(status_value)

         end program


