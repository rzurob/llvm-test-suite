!*********************************************************************
! %START
! %MAIN: YES
! %PRECMD: $TR_SRC/fxi3e.presh fi3ehltmod07
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
!*  PRIMARY FUNCTIONS TESTED   : IEEE_SET_HALTING_MODE,IEEE_GET_HALTING_MODE
!*
!*  SECONDARY FUNCTIONS TESTED : IEEE_SET_STATUS,IEEE_GET_STATUS
!*                               IEEE_SET_FLAG,IEEE_GET_FLAG
!*
!*  DRIVER STANZA              : xlf95
!*  REQUIRED COMPILER OPTIONS  : -qfloat=nofold -qflttrap=zerodivide -qsigtrap
!*
!*  KEYWORD(S)                 : 
!*
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : Testing IEEE_SET_HALTING_MODE and 
!*                               IEEE_GET_HALTING_MODE subroutines for 
!*                               REAL*8. Halting on IEEE_DIVIDE_BY_ZERO. 
!*                               
!234567890123456789012345678901234567890123456789012345678901234567890
       
         program fi3ehltmod07

         use ieee_exceptions
         use constants_for_ieee 

         real(8) :: tr_8
         logical :: actual_flag_value, actual_halting_value
         integer :: caseid

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
         call ieee_set_flag(IEEE_DIVIDE_BY_ZERO, .false. )
         call ieee_get_flag(IEEE_DIVIDE_BY_ZERO, actual_flag_value)
         if (actual_flag_value .neqv. .false. ) then
            call zzrc(caseid+1)
         endif

         call ieee_get_halting_mode(IEEE_DIVIDE_BY_ZERO, actual_halting_value)
         if ( actual_halting_value .neqv. .false. ) then
            call zzrc(caseid+2)
         endif

         call ieee_set_halting_mode(IEEE_DIVIDE_BY_ZERO, .true.)

         tr_8 = r2_8/r1_8
         print *, tr_8

         call ieee_get_flag(IEEE_DIVIDE_BY_ZERO, actual_flag_value)
         if ( actual_flag_value .neqv. .true. ) then
            call zzrc(caseid+3)
         endif
         if ( tr_8 /= PINF_8) then
            call zzrc(caseid+4)
         endif

         end program


