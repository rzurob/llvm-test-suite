!*********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: fpscrrnd02.f
! %VERIFY:
! %STDIN:
! %STDOUT: fpscrrnd02.out
! %EXECARGS:
! %POSTCMD: rm -f fpscrrnd02.out
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March 30, 2002
!*
!*  PRIMARY FUNCTIONS TESTED   : ieee_arithmetic
!*				 ieee_get_rounding_mode()
!*				 ieee_set_rounding_mode()
!*
!*  REFERENCE                  : Feature 180920
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : This is a FPSCR testcase.
!*				 In this testcase, main program that uses IEEE
!*				 calls internal subroutines which itself calls
!*				 other subroutines which are internal or external.
!*
!*                               Rounding mode is ieee_down on
!*                               entry into subroutine called by main program.
!*
!*				 Rule:
!*  				 1) When returning from a procedure that
!*				 uses IEEE, the settings for
!*  				 rounding mode return to the values
!*				 they had at procedure entry.
!*
!*                               2) Calls to procedures that do not use
!*                               IEEE from procedures that do, the
!*                               floating-point status will not change.
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

	program fpscrrnd02
	  use ieee_arithmetic
	  implicit none
	  type(ieee_round_type) :: round_value

!*  Set rounding mode to ieee_down.
  	  call ieee_set_rounding_mode(ieee_down)
  	  call ieee_get_rounding_mode(round_value)
  	  if (round_value /= ieee_down) 		error stop 1


!***********************************************************************
!*** Rounding mode is ieee_down on entry into subroutine;
!*** so on return rounding mode will be ieee_down.
!***********************************************************************

!*  Main program uses IEEE, and calls subroutine that uses IEEE.
          call int_sub102()
          call ieee_get_rounding_mode(round_value)
          if (round_value /= ieee_down)              error stop 2

          call int_sub103()
          call ieee_get_rounding_mode(round_value)
          if (round_value /= ieee_down)              error stop 3

!*  Main program uses IEEE, and calls subroutine that uses IEEE,
!*  which calls other subroutines that do or don't use IEEE.
	  call int_sub101()
          call ieee_get_rounding_mode(round_value)
          if (round_value /= ieee_down)              error stop 4

	contains


!***********************************************************************
!*  Subroutine that use IEEE modules calls other subroutines.
!***********************************************************************
        subroutine int_sub101()

!***  Subroutines that use IEEE modules.
          call ieee_get_rounding_mode(round_value)
          if (round_value /= ieee_down)              error stop 10

	  call int_sub102()
          call ieee_get_rounding_mode(round_value)
          if (round_value /= ieee_down)              error stop 11

	  call int_sub103()
          call ieee_get_rounding_mode(round_value)
          if (round_value /= ieee_down)              error stop 12

!***  Subroutines that do not use IEEE modules.
	  call ext_sub202()
          call ieee_get_rounding_mode(round_value)
          if (round_value /= ieee_down)              error stop 14

	  call ext_sub203()
          call ieee_get_rounding_mode(round_value)
          if (round_value /= ieee_down)              error stop 15

        end subroutine !** end int_sub101()

!* ---------------------------------------------------------------------

!***********************************************************************
!*  Subroutine that use IEEE modules.
!***********************************************************************
!* In int_sub102, rounding mode is set to ieee_up
        subroutine int_sub102()
          use ieee_arithmetic
          logical*4 flag_values(5)
          type(ieee_round_type) :: round_value

          call ieee_set_rounding_mode(ieee_up)
          call ieee_get_rounding_mode(round_value)
          if (round_value /= ieee_up)                   error stop 102

        end subroutine !** end int_sub102()
!* ---------------------------------------------------------------------

!* In int_sub103, rounding mode is set to ieee_to_zero then set to
!* ieee_down.
        subroutine int_sub103()
          use ieee_arithmetic
          type(ieee_round_type) :: round_value

          call ieee_set_rounding_mode(ieee_to_zero)
          call ieee_get_rounding_mode(round_value)
          if (round_value /= ieee_to_zero)              error stop 104

          call ieee_set_rounding_mode(ieee_nearest)
          call ieee_get_rounding_mode(round_value)
          if (round_value /= ieee_nearest)              error stop 105

        end subroutine !** end int_sub103()
!* ---------------------------------------------------------------------

	end program


!=======================================================================
!***********************************************************************
!*  External Subroutines that do not use IEEE modules.
!* --------------------------------------------------------------
!*  Rule:
!*                               Calls to procedures that do not use
!*                               IEEE from procedures that do, the
!*                               floating-point status will not change.
!***********************************************************************
!* In ext_sub202, rounding mode is set to fp_rnd_rm
        subroutine ext_sub202()
          use xlf_fp_util
	  integer(fp_mode_kind) :: round_value

	  round_value = set_round_mode(fp_rnd_rm)
  	  if (get_round_mode() /= fp_rnd_rm)		error stop 202

        end subroutine !** end ext_sub202()
!* ---------------------------------------------------------------------

!* In ext_sub203, rounding mode is set to fp_rnd_rp and then to fp_rnd_rz
        subroutine ext_sub203()
          use xlf_fp_util
          integer(fp_mode_kind) :: round_value

          round_value = set_round_mode(fp_rnd_rp)
          if (get_round_mode() /= fp_rnd_rp)         error stop 203

          round_value = set_round_mode(fp_rnd_rz)
          if (get_round_mode() /= fp_rnd_rz)         error stop 204

        end subroutine !** end ext_sub203()

!=======================================================================

