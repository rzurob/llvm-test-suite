! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: $TR_SRC/fxieee.presh fxieee04 
! %COMPOPTS:  -qfree=f90
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
!*  PROGRAMMER                 : Marcus Yu
!*  DATE                       : February 5, 2002
!*  ORIGIN                     : XL Fortran Development
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : IEEE_SET_HALTING_MODE
!*  SECONDARY FUNCTIONS TESTED : IEEE_GET_HALTING_MODE
!*                               IEEE_SUPPORT_HALTING_MODE
!*
!*  DRIVER STANZA              : xlf90
!*  REQUIRED COMPILER OPTIONS  : -qflttrap
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : Test set_halting_mode to false. TC 24 to 38
!*                               will test set_halting_mode to true.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
	     program fxieee04

         use ieee_exceptions
         use constants_for_ieee

         real :: xr, yr, zr, tmp
		 real*8 :: xr_8, yr_8, zr_8, r8, tmp_8
		 real*16 :: xr_16, yr_16, zr_16, r16, tmp_16
		 logical :: all_flags(5), usual_flags(3), flag
		 logical :: expected_all(5), expected_usual(3)
         integer :: i
         type(ieee_status_type) :: status_value

!   save the original status
	     call ieee_get_status(status_value)

        ! continue if support halting
         if (ieee_support_halting(ieee_invalid) .eqv. .false.) then
           stop "No support halting on invalid."
         endif

         if (ieee_support_halting(ieee_overflow) .eqv. .false.) then
		     stop "No support halting on overflow."
         endif
				
         if (ieee_support_halting(ieee_divide_by_zero) .eqv. .false.) then
		     stop "No support halting on divide_by_zero."
         endif
		
         if (ieee_support_halting(ieee_underflow) .eqv. .false.) then
		     stop "No support halting on underflow."
         endif
		
		 if (ieee_support_halting(ieee_inexact) .eqv. .false.) then
		     stop "No support halting on inexact."
         endif
		
		 call ieee_set_halting_mode(ieee_all,.false.)
         call ieee_get_halting_mode(ieee_all, all_flags)
		 expected_all = (/.false., .false., .false., .false., .false./)
		 do i = 1, 5
		   if (all_flags(i) .neqv. expected_all(i)) then
		       print *, "Failed.  halting_modes were set."
		   endif
		 enddo

		  call ieee_set_halting_mode(ieee_all, .true.)
		  call ieee_get_halting_mode(ieee_all, all_flags)
		  expected_all = (/.true., .true., .true., .true., .true./)
		  do i = 1, 5
		     if (all_flags(i) .neqv. expected_all(i)) then
		        print *, "Failed, halting_modes were not set."
		     endif
		  enddo

		  call ieee_set_halting_mode(ieee_usual,.false.)
		  call ieee_get_halting_mode(ieee_usual, usual_flags)
		  expected_usual = (/.false., .false., .false./)
		  do i = 1, 3
		     if (usual_flags(i) .neqv. expected_usual(i)) then
		        print *, "Failed, halting_modes were set."
		  	  endif
		  enddo

		  call ieee_set_halting_mode(ieee_usual, .true.)
		  call ieee_get_halting_mode(ieee_usual, usual_flags)
		  expected_usual = (/.true., .true., .true./)
		  do i = 1, 3
		     if (usual_flags(i) .neqv. expected_usual(i)) then
		  		  print *, "Failed, halting_modes were not set."
		  	  endif
		  enddo
		  
		  call ieee_set_halting_mode(ieee_divide_by_zero, .true.)
		  call ieee_get_halting_mode(ieee_divide_by_zero, flag)
		  if (flag .eqv. .false.) then
		  	  print *, "Failed, halting_mode divide by zero were not set."
		  endif
		  
		  call ieee_set_halting_mode(ieee_overflow, .true.)
		  call ieee_get_halting_mode(ieee_overflow, flag)
		  if (flag .eqv. .false.) then
		  	  print *, "Failed, halting_mode overflow were not set."
		  endif
		  
		  call ieee_set_halting_mode(ieee_invalid, .true.)
		  call ieee_get_halting_mode(ieee_invalid, flag)
		  if (flag .eqv. .false.) then
		  	  print *, "Failed, halting_mode invalid were not set."
		  endif
		  
		  call ieee_set_halting_mode(ieee_underflow, .true.)
		  call ieee_get_halting_mode(ieee_underflow, flag)
		  if (flag .eqv. .false.) then
		  	  print *, "Failed, halting_mode underflow were not set."
		  endif
		  
		  call ieee_set_halting_mode(ieee_inexact, .true.)
		  call ieee_get_halting_mode(ieee_inexact, flag)
		  if (flag .eqv. .false.) then
		  	  print *, "Failed, halting_mode inexact were not set."
		  endif

! restore the original falgs.
        call ieee_set_status(status_value)

        end program
