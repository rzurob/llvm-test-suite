! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: $TR_SRC/fxieee.presh fxieee14
! %COMPOPTS: -qfree=f90
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
!*  DATE                       : February 8, 2002
!*  ORIGIN                     : XL Fortran Development
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : IEEE_SET_FLAG
!*  SECONDARY FUNCTIONS TESTED : IEEE_GET_FLAG
!*
!*  DRIVER STANZA              : xlf90
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*
!234567890123456789012345678901234567890123456789012345678901234567890
	     program fxieee14

        use ieee_exceptions
        use constants_for_ieee

        logical(4) :: all_flags(5), usual_flags(3), original(5)
        logical(4) :: expected_all(5), expected_usual(3)

        integer :: i

        call ieee_get_flag(ieee_all, original)

        call ieee_set_flag(ieee_all,.false.)
        call ieee_get_flag(ieee_all, all_flags)
        expected_all = (/.false., .false., .false., .false., .false./)
        do i = 1, 5
          if (all_flags(i) .neqv. expected_all(i)) then
            print *, "Failed 1.  Exception flags all were set."
          endif
        enddo

        call ieee_set_flag(ieee_all, .true.)
        call ieee_get_flag(ieee_all, all_flags)
        expected_all = (/.true., .true., .true., .true., .true./)
        do i = 1, 5
          if (all_flags(i) .neqv. expected_all(i)) then
            print *, "Failed 2, Exception flags all were not set."
          endif
        enddo

        expected_usual = (/.false., .false., .false./)
        call ieee_set_flag(ieee_usual, expected_usual)
		call ieee_get_flag(ieee_usual, usual_flags)
		do i = 1, 3
		   if (usual_flags(i) .neqv. expected_usual(i)) then
		      print *, "Failed 3.  Exception flags usual were set."
		   endif
		enddo

		  call ieee_set_flag(ieee_usual, .true.)
		  call ieee_get_flag(ieee_usual, usual_flags)
		  expected_usual = (/.true., .true., .true./)
		  do i = 1, 3
		     if (usual_flags(i) .neqv. expected_usual(i)) then
		        print *, "Failed 4, Exception flags usual were not set."
		     endif
        enddo

        call ieee_set_flag(ieee_all,.false.)

        call ieee_set_flag(IEEE_OVERFLOW, .true.)
        call ieee_get_flag(IEEE_OVERFLOW, all_flags(1))
        if (all_flags(1) .neqv. .true. ) then
           print *, "Failed 5, OVERFLOW was not set."
        endif
		
		call ieee_set_flag(IEEE_UNDERFLOW, .true.)
        call ieee_get_flag(IEEE_UNDERFLOW, all_flags(1))
        if (all_flags(1) .neqv. .true. ) then
           print *, "Failed 6, UNDERFLOW was not set."
        endif
		
		call ieee_set_flag(IEEE_DIVIDE_BY_ZERO, .true.)
        call ieee_get_flag(IEEE_DIVIDE_BY_ZERO, all_flags(1))
        if (all_flags(1) .neqv. .true. ) then
           print *, "Failed 7, DIVIDE_BY_ZERO was not set."
        endif
		
		call ieee_set_flag(IEEE_INVALID, .true.)
        call ieee_get_flag(IEEE_INVALID, all_flags(1))
        if (all_flags(1) .neqv. .true. ) then
           print *, "Failed 8, INVALID was not set."
        endif
		
		call ieee_set_flag(IEEE_INEXACT, .true.)
        call ieee_get_flag(IEEE_INEXACT, all_flags(1))
        if (all_flags(1) .neqv. .true. ) then
           print *, "Failed 9, INEXACT was not set."
        endif
		
       call ieee_set_flag(ieee_all,.true.)

        call ieee_set_flag(IEEE_OVERFLOW, .false.)
        call ieee_get_flag(IEEE_OVERFLOW, all_flags(1))
        if (all_flags(1) .eqv. .true. ) then
           print *, "Failed 10, OVERFLOW was set."
        endif
		
		call ieee_set_flag(IEEE_UNDERFLOW, .false.)
        call ieee_get_flag(IEEE_UNDERFLOW, all_flags(1))
        if (all_flags(1) .eqv. .true. ) then
           print *, "Failed 11, UNDERFLOW was set."
        endif
		
		call ieee_set_flag(IEEE_DIVIDE_BY_ZERO, .false.)
        call ieee_get_flag(IEEE_DIVIDE_BY_ZERO, all_flags(1))
        if (all_flags(1) .eqv. .true. ) then
           print *, "Failed 12, DIVIDE_BY_ZERO was set."
        endif
		
		call ieee_set_flag(IEEE_INVALID, .false.)
        call ieee_get_flag(IEEE_INVALID, all_flags(1))
        if (all_flags(1) .eqv. .true. ) then
           print *, "Failed 13, INVALID was set."
        endif
		
		call ieee_set_flag(IEEE_INEXACT, .false.)
        call ieee_get_flag(IEEE_INEXACT, all_flags(1))
        if (all_flags(1) .eqv. .true. ) then
           print *, "Failed 14, INEXACT was not set."
        endif		
		
		call ieee_set_flag(ieee_all, original)

        end
