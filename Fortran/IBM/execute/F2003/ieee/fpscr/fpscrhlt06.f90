!*********************************************************************
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
!*				 calls subroutines which itself calls another
!*				 subroutine.
!*
!*                               All IEEE halting flags are true on
!*                               entry into subroutine called by main program.
!*
!*				 Rule:
!*  				 1) When returning from a procedure that
!*				 uses IEEE, the settings for
!*  				 halting mode return to the values
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

	program fpscrhlt06
	  use ieee_arithmetic
	  implicit none

          interface

            subroutine ext_sub101()
	  	use ieee_arithmetic
            end subroutine

            subroutine ext_sub201()
	  	use xlf_fp_util
            end subroutine

          end interface

	  logical*4 flag_values(5)

!*  Set halting mode flags to true.
	  call ieee_set_halting_mode(ieee_all, .true.)
	  call ieee_get_halting_mode(ieee_all, flag_values)
  	  if (any(flag_values .neqv. .true.))		error stop 1

!***********************************************************************
!*** Halting mode flags true on entry into subroutine;
!*** so on return halting mode will flag true.
!***********************************************************************
!*  Main program uses IEEE, and calls subroutine that uses IEEE,
!*  which calls other subroutines that do or don't use IEEE.
	  call ext_sub101()
	  call ieee_get_halting_mode(ieee_all, flag_values)
  	  if (any(flag_values .neqv. .true.))		error stop 2

!*  Main program uses IEEE, and calls subroutine that does not use IEEE,
!*  which calls other subroutines that do or don't use IEEE.
	  call ext_sub201()
	  call ieee_get_halting_mode(ieee_all, flag_values)
  	  if (any(flag_values .neqv. .true.))		error stop 3

	end program


!***********************************************************************
!*  Subroutine that use IEEE modules calls other subroutines.
!***********************************************************************
        subroutine ext_sub101()
          use ieee_arithmetic
	  logical*4 flag_values(5)

	  interface
            subroutine ext_sub102()
	  	use ieee_arithmetic
            end subroutine

            subroutine ext_sub103()
	  	use ieee_arithmetic
            end subroutine

            subroutine ext_sub104()
	  	use ieee_arithmetic
            end subroutine

            subroutine ext_sub202()
                use xlf_fp_util
            end subroutine

            subroutine ext_sub203()
                use xlf_fp_util
            end subroutine

            subroutine ext_sub204()
                use xlf_fp_util
            end subroutine
	  end interface

!***  Subroutines that use IEEE modules.
	  call ieee_get_halting_mode(ieee_all, flag_values)
  	  if (any(flag_values .neqv. .true.))		error stop 10

	  call ext_sub102()
	  call ieee_get_halting_mode(ieee_all, flag_values)
  	  if (any(flag_values .neqv. .true.))		error stop 11

	  call ext_sub103()
	  call ieee_get_halting_mode(ieee_all, flag_values)
  	  if (any(flag_values .neqv. .true.))		error stop 12

	  call ext_sub104()
	  call ieee_get_halting_mode(ieee_all, flag_values)
  	  if (any(flag_values .neqv. .true.))		error stop 13

!***  Subroutines that do not use IEEE modules.
	  call ext_sub202()
	  call ieee_get_halting_mode(ieee_all, flag_values)
  	  if (any(flag_values .neqv. .true.))		error stop 14

	  call ext_sub203()
	  call ieee_get_halting_mode(ieee_all, flag_values)
  	  if (any(flag_values .neqv. .true.))		error stop 15

	  call ext_sub204()
	  call ieee_get_halting_mode(ieee_all, flag_values)
  	  if (any(flag_values .neqv. .true.))		error stop 16

        end subroutine !** end ext_sub101()
!* ---------------------------------------------------------------------
!***********************************************************************
!*  Subroutine that does not use IEEE modules calls other subroutines.
!***********************************************************************
        subroutine ext_sub201()
          use xlf_fp_util
          integer*4 flag_values(5)

          interface
            subroutine ext_sub102()
                use ieee_arithmetic
            end subroutine

            subroutine ext_sub103()
                use ieee_arithmetic
            end subroutine

            subroutine ext_sub104()
                use ieee_arithmetic
            end subroutine

            subroutine ext_sub202()
                use xlf_fp_util
            end subroutine

            subroutine ext_sub203()
                use xlf_fp_util
            end subroutine

            subroutine ext_sub204()
                use xlf_fp_util
            end subroutine
          end interface

          flag_values(1) = get_fpscr_flags(trp_overflow)
          flag_values(2) = get_fpscr_flags(trp_div_by_zero)
          flag_values(3) = get_fpscr_flags(trp_invalid)
          flag_values(4) = get_fpscr_flags(trp_underflow)
          flag_values(5) = get_fpscr_flags(trp_inexact)
          if (any(flag_values .eq. 0 ))          error stop 20


!***  Subroutines that use IEEE modules.
!*  Rule:  When returning from a procedure that uses IEEE, the settings for
!*	   halting mode return to the values they had at procedure entry.

          call ext_sub102()
          flag_values(1) = get_fpscr_flags(trp_overflow)
          flag_values(2) = get_fpscr_flags(trp_div_by_zero)
          flag_values(3) = get_fpscr_flags(trp_invalid)
          flag_values(4) = get_fpscr_flags(trp_underflow)
          flag_values(5) = get_fpscr_flags(trp_inexact)
          if (any(flag_values .eq. 0 ))          error stop 21

          call ext_sub103()
          flag_values(1) = get_fpscr_flags(trp_overflow)
          flag_values(2) = get_fpscr_flags(trp_div_by_zero)
          flag_values(3) = get_fpscr_flags(trp_invalid)
          flag_values(4) = get_fpscr_flags(trp_underflow)
          flag_values(5) = get_fpscr_flags(trp_inexact)
          if (any(flag_values .eq. 0 ))          error stop 22


          call ext_sub104()
          flag_values(1) = get_fpscr_flags(trp_overflow)
          flag_values(2) = get_fpscr_flags(trp_div_by_zero)
          flag_values(3) = get_fpscr_flags(trp_invalid)
          flag_values(4) = get_fpscr_flags(trp_underflow)
          flag_values(5) = get_fpscr_flags(trp_inexact)
          if (any(flag_values .eq. 0 ))          error stop 23
!* ---------------------------------------------------------------------

!***  Subroutines that do not use IEEE modules.
!*  Rule:  When returning from a procedure that does not use IEEE,
!*         the settings for halting mode return to the values
!*         they had at the exit of the procedure

          call ext_sub202()
          flag_values(1) = get_fpscr_flags(trp_overflow)
          flag_values(2) = get_fpscr_flags(trp_div_by_zero)
          flag_values(3) = get_fpscr_flags(trp_invalid)
          flag_values(4) = get_fpscr_flags(trp_underflow)
          flag_values(5) = get_fpscr_flags(trp_inexact)
          if (any(flag_values .ne. 0 ))          error stop 24

!*  Set flags on entry into subroutine
          call set_fpscr_flags(trp_overflow)
          call set_fpscr_flags(trp_div_by_zero)
          call set_fpscr_flags(trp_invalid)
          call set_fpscr_flags(trp_underflow)
          call set_fpscr_flags(trp_inexact)

          call ext_sub203()
          flag_values(1) = get_fpscr_flags(trp_overflow)
          flag_values(2) = get_fpscr_flags(trp_div_by_zero)
          flag_values(3) = get_fpscr_flags(trp_invalid)
          flag_values(4) = get_fpscr_flags(trp_underflow)
          flag_values(5) = get_fpscr_flags(trp_inexact)
          if (flag_values(1) .ne. 0)            error stop 25
          if (flag_values(2) .eq. 0)            error stop 26
          if (flag_values(3) .ne. 0)            error stop 27
          if (flag_values(4) .eq. 0)            error stop 28
          if (flag_values(5) .ne. 0)            error stop 29

!*  Set flags on entry into subroutine
          call set_fpscr_flags(trp_overflow)
          call set_fpscr_flags(trp_div_by_zero)
          call set_fpscr_flags(trp_invalid)
          call set_fpscr_flags(trp_underflow)
          call set_fpscr_flags(trp_inexact)

          call ext_sub204()
          flag_values(1) = get_fpscr_flags(trp_overflow)
          flag_values(2) = get_fpscr_flags(trp_div_by_zero)
          flag_values(3) = get_fpscr_flags(trp_invalid)
          flag_values(4) = get_fpscr_flags(trp_underflow)
          flag_values(5) = get_fpscr_flags(trp_inexact)
          if (flag_values(1) .eq. 0)            error stop 30
          if (flag_values(2) .ne. 0)            error stop 31
          if (flag_values(3) .eq. 0)            error stop 32
          if (flag_values(4) .ne. 0)            error stop 33
          if (flag_values(5) .eq. 0)            error stop 34

        end subroutine !** end ext_sub201()


!***********************************************************************
!*  Subroutine that use IEEE modules.
!* --------------------------------------------------------------
!*  Rule:
!*  When returning from a procedure that uses IEEE, the settings for
!*  halting mode return to the values they had at procedure entry.
!***********************************************************************
!* In ext_sub102, all halting flags are set to false
	subroutine ext_sub102()
          use ieee_arithmetic
          logical*4 flag_values(5)

          call ieee_set_halting_mode(ieee_all, .false.)
          call ieee_get_halting_mode(ieee_all, flag_values)
          if (any(flag_values .neqv. .false.))          error stop 102

        end subroutine !** end ext_sub102()
!* ---------------------------------------------------------------------

!* In ext_sub103, some halting flags are set to flase; while some are
!* not modified.
        subroutine ext_sub103()
          use ieee_arithmetic
          logical*4 flag_values(5)

          call ieee_set_halting_mode(ieee_divide_by_zero, .false.)
          call ieee_set_halting_mode(ieee_underflow, .false.)

          call ieee_get_halting_mode(ieee_all, flag_values)
          if (flag_values(1) .neqv. .true.)          	error stop 113
          if (flag_values(2) .neqv. .false.)          	error stop 123
          if (flag_values(3) .neqv. .true.)          	error stop 133
          if (flag_values(4) .neqv. .false.)          	error stop 143
          if (flag_values(5) .neqv. .true.)          	error stop 153

        end subroutine !** end ext_sub103()
!* ---------------------------------------------------------------------

!* In ext_sub104, the halting flags that were false in ext_sub103 are
!* not modified(remain true); and the remaining flags are set to flase.
        subroutine ext_sub104()
          use ieee_arithmetic
          logical*4 flag_values(5)


          call ieee_set_halting_mode(ieee_overflow, .false.)
          call ieee_set_halting_mode(ieee_invalid, .false.)
          call ieee_set_halting_mode(ieee_inexact, .false.)

          call ieee_get_halting_mode(ieee_all, flag_values)
          if (flag_values(1) .neqv. .false.)            error stop 114
          if (flag_values(2) .neqv. .true.)             error stop 124
          if (flag_values(3) .neqv. .false.)            error stop 134
          if (flag_values(4) .neqv. .true.)             error stop 144
          if (flag_values(5) .neqv. .false.)            error stop 154

        end subroutine !** end ext_sub104()

!=======================================================================
!***********************************************************************
!*  Subroutines that do not use IEEE modules.
!* --------------------------------------------------------------
!*  Rule:
!*                               Calls to procedures that do not use
!*                               IEEE from procedures that do, the
!*                               floating-point status will not change.
!***********************************************************************
!* In ext_sub202, all halting flags are cleared.
        subroutine ext_sub202()
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

          if (any(flag_values .ne. 0 ))          error stop 202

        end subroutine !** end ext_sub202()
!* ---------------------------------------------------------------------

!* In ext_sub203, some halting flags are cleared; while some are
!* not modified.
        subroutine ext_sub203()
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

          if (flag_values(1) .ne. 0)            error stop 213
          if (flag_values(2) .eq. 0)            error stop 223
          if (flag_values(3) .ne. 0)            error stop 233
          if (flag_values(4) .eq. 0)            error stop 243
          if (flag_values(5) .ne. 0)            error stop 253

        end subroutine !** end ext_sub203()
!* ---------------------------------------------------------------------

!* In ext_sub204, the halting flags that were cleared in ext_sub203 are
!* not modified; and the remaining flags are cleared.
        subroutine ext_sub204()
          use xlf_fp_util
          integer*4 flag_values(5)

          call clr_fpscr_flags(trp_div_by_zero)
          call clr_fpscr_flags(trp_underflow)

          flag_values(1) = get_fpscr_flags(trp_overflow)
          flag_values(2) = get_fpscr_flags(trp_div_by_zero)
          flag_values(3) = get_fpscr_flags(trp_invalid)
          flag_values(4) = get_fpscr_flags(trp_underflow)
          flag_values(5) = get_fpscr_flags(trp_inexact)

          if (flag_values(1) .eq. 0)            error stop 214
          if (flag_values(2) .ne. 0)            error stop 224
          if (flag_values(3) .eq. 0)            error stop 234
          if (flag_values(4) .ne. 0)            error stop 244
          if (flag_values(5) .eq. 0)            error stop 254

        end subroutine !** end ext_sub204()

!=======================================================================

