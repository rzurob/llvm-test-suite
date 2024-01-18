!*********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: fpscrhlt04.f
! %VERIFY:
! %STDIN:
! %STDOUT: fpscrhlt04.out
! %EXECARGS:
! %POSTCMD: rm -f fpscrhlt04.out
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March 30, 2002
!*
!*  PRIMARY FUNCTIONS TESTED   : ieee_arithmetic
!*				 ieee_get_halting_mode()
!*				 ieee_set_halting_mode()
!*
!*  REFERENCE                  : Feature 180920
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : This is a FPSCR testcase.
!*				 In this testcase, main program that uses IEEE
!*				 calls subroutines does not uses IEEE
!*
!*                               All IEEE halting flags are true on
!*                               entry into subroutine.
!*
!*				 Rule:
!*  				 Calls to procedures that do not use
!*				 IEEE from procedures that do, the
!*  				 floating-point status will not change.
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

	program fpscrhlt04
	  use ieee_arithmetic
	  implicit none

          interface

            subroutine ext_sub101()
	  	use xlf_fp_util
            end subroutine

            subroutine ext_sub102()
	  	use xlf_fp_util
            end subroutine

            subroutine ext_sub103()
	  	use xlf_fp_util
            end subroutine

            subroutine ext_sub104()
	  	use xlf_fp_util
            end subroutine

          end interface

	  logical*4 flag_values(5)

!*  Set halting mode flags to true.
	  call ieee_set_halting_mode(ieee_all, .true.)
	  call ieee_get_halting_mode(ieee_all, flag_values)
  	  if (any(flag_values .neqv. .true.))		error stop 1

!***********************************************************************
!*  Main program uses IEEE, and calls subroutine that don't use IEEE
!* --------------------------------------------------------------
!*  Rule:
!*  				 Calls to procedures that do not use
!*				 IEEE from procedures that do, the
!*  				 floating-point status will not change.
!***********************************************************************
!*** Test1:  Halting mode flags true on entry into subroutine;
!*** 	     so on return halting mode will flag true.

!* In ext_sub101, halting mode is not modified.
	  call ext_sub101()
	  call ieee_get_halting_mode(ieee_all, flag_values)
  	  if (any(flag_values .neqv. .true.))		error stop 2

!* In ext_sub102, all halting flags are set to false.
	  call ext_sub102()
	  call ieee_get_halting_mode(ieee_all, flag_values)
  	  if (any(flag_values .neqv. .true.))		error stop 3

!* In ext_sub103, some halting flags are set to true; while some are
!* not modified.
	  call ext_sub103()
	  call ieee_get_halting_mode(ieee_all, flag_values)
  	  if (any(flag_values .neqv. .true.))		error stop 4

!* In ext_sub104, the halting flags that were true in ext_sub103 are
!* not modified (so flags are false); and the remaining flags are set to true.
	  call ext_sub104()
	  call ieee_get_halting_mode(ieee_all, flag_values)
  	  if (any(flag_values .neqv. .true.))		error stop 5

	end program


!***********************************************************************
!*  Subroutine that do not IEEE modules.
!* --------------------------------------------------------------
!*  Rule:
!*  				 Calls to procedures that do not use
!*				 IEEE from procedures that do, the
!*  				 floating-point status will not change.
!***********************************************************************

!* In ext_sub101, halting mode is not modified.
        subroutine ext_sub101()
          use xlf_fp_util
	  integer*4 flag_values(5)

          flag_values(1) = get_fpscr_flags(trp_overflow)
          flag_values(2) = get_fpscr_flags(trp_div_by_zero)
          flag_values(3) = get_fpscr_flags(trp_invalid)
          flag_values(4) = get_fpscr_flags(trp_underflow)
          flag_values(5) = get_fpscr_flags(trp_inexact)

          if (any(flag_values .eq. 0 ))          error stop 101

        end subroutine !** end ext_sub101()
!* ---------------------------------------------------------------------

!* In ext_sub102, all halting flags are cleared.
	subroutine ext_sub102()
          use xlf_fp_util
          integer*4 flag_values(5)

          call clr_fpscr_flags(trp_overflow)
          call clr_fpscr_flags(trp_div_by_zero)
          call clr_fpscr_flags(trp_invalid)
          call clr_fpscr_flags(trp_underflow)
          call clr_fpscr_flags(trp_inexact)

          flag_values(1) = get_fpscr_flags(trp_overflow)
          flag_values(2) = get_fpscr_flags(trp_div_by_zero)
          flag_values(3) = get_fpscr_flags(trp_invalid)
          flag_values(4) = get_fpscr_flags(trp_underflow)
          flag_values(5) = get_fpscr_flags(trp_inexact)

          if (any(flag_values .ne. 0 ))          error stop 102

        end subroutine !** end ext_sub102()
!* ---------------------------------------------------------------------

!* In ext_sub103, some halting flags are cleared; while some are
!* not modified.
        subroutine ext_sub103()
          use xlf_fp_util
          integer*4 flag_values(5)

          call clr_fpscr_flags(trp_overflow)
          call clr_fpscr_flags(trp_invalid)
          call clr_fpscr_flags(trp_inexact)

          flag_values(1) = get_fpscr_flags(trp_overflow)
          flag_values(2) = get_fpscr_flags(trp_div_by_zero)
          flag_values(3) = get_fpscr_flags(trp_invalid)
          flag_values(4) = get_fpscr_flags(trp_underflow)
          flag_values(5) = get_fpscr_flags(trp_inexact)

          if (flag_values(1) .ne. 0)      	error stop 113
          if (flag_values(2) .eq. 0)            error stop 123
          if (flag_values(3) .ne. 0)            error stop 133
          if (flag_values(4) .eq. 0)            error stop 143
          if (flag_values(5) .ne. 0)            error stop 153

        end subroutine !** end ext_sub103()
!* ---------------------------------------------------------------------

!* In ext_sub104, the halting flags that were clr in ext_sub103 are
!* not modified; and the remaining flags are cleared.
        subroutine ext_sub104()
          use xlf_fp_util
          integer*4 flag_values(5)

          call clr_fpscr_flags(trp_div_by_zero)
          call clr_fpscr_flags(trp_underflow)

          flag_values(1) = get_fpscr_flags(trp_overflow)
          flag_values(2) = get_fpscr_flags(trp_div_by_zero)
          flag_values(3) = get_fpscr_flags(trp_invalid)
          flag_values(4) = get_fpscr_flags(trp_underflow)
          flag_values(5) = get_fpscr_flags(trp_inexact)

          if (flag_values(1) .eq. 0)      	error stop 114
          if (flag_values(2) .ne. 0)            error stop 124
          if (flag_values(3) .eq. 0)            error stop 134
          if (flag_values(4) .ne. 0)            error stop 144
          if (flag_values(5) .eq. 0)            error stop 154

        end subroutine !** end ext_sub104()

!=======================================================================
