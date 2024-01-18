!*********************************************************************
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March 30, 2002
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*				 ieee_get_rounding_mode()
!*				 ieee_set_rounding_mode()
!*
!*  REFERENCE                  : Feature 180920
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : This is a FPSCR testcase.
!*				 In this testcase, main program that uses IEEE
!*				 calls subroutines which itself calls another
!*				 subroutine from a module.
!*
!*                               Rounding mode is ieee_up on
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

module withieee
        use ieee_arithmetic
        contains
!***********************************************************************
!*  Subroutines that use IEEE modules.
!***********************************************************************
!* In ext_sub102, rounding mode is set to ieee_up
        subroutine ext_sub102()
          logical*4 flag_values(5)
          type(ieee_round_type) :: round_value

          call ieee_set_rounding_mode(ieee_nearest)
          call ieee_get_rounding_mode(round_value)
          if (round_value /= ieee_nearest)    		error stop 102

        end subroutine !** end ext_sub102()
!* ---------------------------------------------------------------------

!* In ext_sub103, rounding mode is set to ieee_to_zero then set to
!* ieee_down.
        subroutine ext_sub103()
          type(ieee_round_type) :: round_value

          call ieee_set_rounding_mode(ieee_to_zero)
          call ieee_get_rounding_mode(round_value)
          if (round_value /= ieee_to_zero)              error stop 103

          call ieee_set_rounding_mode(ieee_down)
          call ieee_get_rounding_mode(round_value)
          if (round_value /= ieee_down)                 error stop 104

        end subroutine !** end ext_sub103()
!* ---------------------------------------------------------------------
end module

!=======================================================================

module withoutieee
        use xlf_fp_util
        contains
!***********************************************************************
!*  Subroutines that do not use IEEE modules.
!***********************************************************************
!* In ext_sub202, rounding mode is set to fp_rnd_rm
        subroutine ext_sub202()
          integer(fp_mode_kind) :: round_value

          round_value = set_round_mode(fp_rnd_rm)
          if (get_round_mode() /= fp_rnd_rm)            error stop 202

        end subroutine !** end ext_sub202()
!* ---------------------------------------------------------------------

!* In ext_sub203, rounding mode is set to fp_rnd_rp and then to fp_rnd_rz
        subroutine ext_sub203()
          integer(fp_mode_kind) :: round_value

          round_value = set_round_mode(fp_rnd_rn)
          if (get_round_mode() /= fp_rnd_rn)         	error stop 203

          round_value = set_round_mode(fp_rnd_rz)
          if (get_round_mode() /= fp_rnd_rz)         	error stop 204

        end subroutine !** end ext_sub203()
!* ---------------------------------------------------------------------

end module

!=======================================================================

	program fpscrrnd03
!***  Module withieee uses ieee_arithmetic, so using the module in main
!***  program will make main program use ieee_arithmetic.
	  use withieee

!***  Module withoutieee uses xlf_fp_util, so using the module in main
!***  program will make main program use xlf_fp_util.
	  use withoutieee

	  implicit none

          interface
            subroutine ext_sub101()
	  	use ieee_arithmetic
            end subroutine

            subroutine ext_sub201()
	  	use xlf_fp_util
            end subroutine
          end interface

	  type(ieee_round_type) :: round_value

!*  Set rounding mode to ieee_up.
          call ieee_set_rounding_mode(ieee_up)
  	  call ieee_get_rounding_mode(round_value)
  	  if (round_value /= ieee_up) 			error stop 1
  	  if (get_round_mode() /= fp_rnd_rp)		error stop 2


!***********************************************************************
!*** Rounding mode is ieee_up on entry into subroutine;
!*** so on return rounding mode will be ieee_up.
!***********************************************************************

!*  Main program uses IEEE, and calls subroutine that uses IEEE.
          call ext_sub102()
          call ieee_get_rounding_mode(round_value)
          if (round_value /= ieee_up)              	error stop 3
  	  if (get_round_mode() /= fp_rnd_rp)		error stop 4

          call ext_sub103()
          call ieee_get_rounding_mode(round_value)
          if (round_value /= ieee_up)              	error stop 5
          if (get_round_mode() /= fp_rnd_rp)            error stop 6

!* ---------------------------------------------------------------------

!*  Main program uses IEEE, and calls subroutine that does not use IEEE.
          call ext_sub202()
          call ieee_get_rounding_mode(round_value)
          if (round_value /= ieee_up)              	error stop 7
          if (get_round_mode() /= fp_rnd_rp)            error stop 8


          call ext_sub203()
          call ieee_get_rounding_mode(round_value)
          if (round_value /= ieee_up)              	error stop 9
          if (get_round_mode() /= fp_rnd_rp)            error stop 10


!* ---------------------------------------------------------------------

!*  Main program uses IEEE, and calls subroutine that uses IEEE,
!*  which calls other subroutines that do or don't use IEEE
!*  from modules.
	  call ext_sub101()
          call ieee_get_rounding_mode(round_value)
          if (round_value /= ieee_up)              	error stop 11
          if (get_round_mode() /= fp_rnd_rp)            error stop 12
!* ---------------------------------------------------------------------

!*  Main program uses IEEE, and calls subroutine that does not use IEEE,
!*  which calls other subroutines that do or don't use IEEE
!*  from modules.
	  call ext_sub201()
          call ieee_get_rounding_mode(round_value)
          if (round_value /= ieee_up)              	error stop 13
          if (get_round_mode() /= fp_rnd_rp)            error stop 14
!* ---------------------------------------------------------------------

	end program


!***********************************************************************
!*  Subroutine that use IEEE modules calls other subroutines.
!***********************************************************************
        subroutine ext_sub101()
!***  Module withieee uses ieee_arithmetic, so using the module in
!***  subroutine will make subroutine use ieee_arithmetic.
          use withieee

!***  Module withoutieee uses xlf_fp_util, so using the module in
!***  subroutine will make subroutine use xlf_fp_util.
          use withoutieee

	  type(ieee_round_type) :: round_value

!***  Subroutines that use IEEE modules.
          call ieee_get_rounding_mode(round_value)
          if (round_value /= ieee_up)              	error stop 20
          if (get_round_mode() /= fp_rnd_rp)            error stop 21

	  call ext_sub102()
          call ieee_get_rounding_mode(round_value)
          if (round_value /= ieee_up)              	error stop 22
          if (get_round_mode() /= fp_rnd_rp)            error stop 23

	  call ext_sub103()
          call ieee_get_rounding_mode(round_value)
          if (round_value /= ieee_up)              	error stop 24
          if (get_round_mode() /= fp_rnd_rp)            error stop 25

!***  Subroutines that do not use IEEE modules.
	  call ext_sub202()
          call ieee_get_rounding_mode(round_value)
          if (round_value /= ieee_up)              	error stop 26
          if (get_round_mode() /= fp_rnd_rp)            error stop 27

	  call ext_sub203()
          call ieee_get_rounding_mode(round_value)
          if (round_value /= ieee_up)              	error stop 28
          if (get_round_mode() /= fp_rnd_rp)            error stop 29

        end subroutine !** end ext_sub101()
!* ---------------------------------------------------------------------


!***********************************************************************
!*  Subroutine that does not use IEEE modules calls other subroutines.
!***********************************************************************
        subroutine ext_sub201()

!***  Module withoutieee uses xlf_fp_util, so using the module in
!***  subroutine will make subroutine use xlf_fp_util.
          use withoutieee

  	  integer(fp_mode_kind) :: round_value

!*  Check if when entering procedure, the rounding mode doesn't change.
!*  Rounding mode should be: round towards plus infinity
	  round_value = get_round_mode()
  	  if (round_value /= fp_rnd_rp)			error stop 30

!***  Subroutines that do not use IEEE modules.
!*  Rule:  When returning from a procedure that does not use IEEE,
!*         the settings for rounding mode will return the last value
!*         when the procedure exits.

          call ext_sub202()
          round_value = get_round_mode()
          if (round_value /= fp_rnd_rm)            	error stop 31

          call ext_sub203()
          round_value = get_round_mode()
          if (round_value /= fp_rnd_rz)            	error stop 32

        end subroutine !** end ext_sub201()


