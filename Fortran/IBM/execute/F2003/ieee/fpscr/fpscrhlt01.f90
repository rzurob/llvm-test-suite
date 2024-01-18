!*********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: fpscrhlt01.f
! %VERIFY:
! %STDIN:
! %STDOUT: fpscrhlt01.out
! %EXECARGS:
! %POSTCMD: rm -f fpscrhlt01.out
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
!*				 calls subroutines which also uses IEEE
!*
!*                               All IEEE halting flags are false on
!*                               entry into subroutine.
!*
!*				 Rule:
!*  				 When returning from a procedure that
!*				 uses IEEE, the settings for
!*  				 halting mode return to the values
!*				 they had at procedure entry.
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

	program fpscrhlt01
	  use ieee_arithmetic
	  implicit none

          interface

            subroutine ext_sub101()
	  	use ieee_arithmetic
            end subroutine

            subroutine ext_sub102()
	  	use ieee_arithmetic
            end subroutine

            subroutine ext_sub103()
	  	use ieee_arithmetic
            end subroutine

            subroutine ext_sub104()
	  	use ieee_arithmetic
            end subroutine

          end interface

	  logical*4 flag_values(5)

!*  Check if initially halting mode flags false.
	  call ieee_get_halting_mode(ieee_all, flag_values)
  	  if (any(flag_values .neqv. .false.))		error stop 1

!***********************************************************************
!*  Main program uses IEEE, and calls subroutine that uses IEEE
!* --------------------------------------------------------------
!*  Rule:
!*  When returning from a procedure that uses IEEE, the settings for
!*  halting mode return to the values they had at procedure entry.
!***********************************************************************
!*** Test1:  Halting mode flags false on entry into subroutine;
!*** 	     so on return halting mode will flag false.

!* In ext_sub101, halting mode is not modified.
	  call ext_sub101()
	  call ieee_get_halting_mode(ieee_all, flag_values)
  	  if (any(flag_values .neqv. .false.))		error stop 2

!* In ext_sub102, all halting flags are set to true.
	  call ext_sub102()
	  call ieee_get_halting_mode(ieee_all, flag_values)
  	  if (any(flag_values .neqv. .false.))		error stop 3

!* In ext_sub103, some halting flags are set to true; while some are
!* not modified.
	  call ext_sub103()
	  call ieee_get_halting_mode(ieee_all, flag_values)
  	  if (any(flag_values .neqv. .false.))		error stop 4

!* In ext_sub104, the halting flags that were true in ext_sub103 are
!* not modified; and the remaining flags are set to true.
	  call ext_sub104()
	  call ieee_get_halting_mode(ieee_all, flag_values)
  	  if (any(flag_values .neqv. .false.))		error stop 5

	end program


!***********************************************************************
!*  Subroutine that use IEEE modules.
!* --------------------------------------------------------------
!*  Rule:
!*  When returning from a procedure that uses IEEE, the settings for
!*  halting mode return to the values they had at procedure entry.
!***********************************************************************

!* In ext_sub101, halting mode is not modified.
        subroutine ext_sub101()
          use ieee_arithmetic
	  logical*4 flag_values(5)

	  call ieee_get_halting_mode(ieee_all, flag_values)
  	  if (any(flag_values .neqv. .false.))		error stop 101

        end subroutine !** end ext_sub101()
!* ---------------------------------------------------------------------

!* In ext_sub102, all halting flags are set to true.
	subroutine ext_sub102()
          use ieee_arithmetic
          logical*4 flag_values(5)

          call ieee_set_halting_mode(ieee_all, .true.)
          call ieee_get_halting_mode(ieee_all, flag_values)
          if (any(flag_values .neqv. .true.))          error stop 102

        end subroutine !** end ext_sub102()
!* ---------------------------------------------------------------------

!* In ext_sub103, some halting flags are set to true; while some are
!* not modified.
        subroutine ext_sub103()
          use ieee_arithmetic
          logical*4 flag_values(5)

          call ieee_set_halting_mode(ieee_overflow, .true.)
          call ieee_set_halting_mode(ieee_invalid, .true.)
          call ieee_set_halting_mode(ieee_inexact, .true.)

          call ieee_get_halting_mode(ieee_all, flag_values)
          if (flag_values(1) .neqv. .true.)          	error stop 113
          if (flag_values(2) .neqv. .false.)          	error stop 123
          if (flag_values(3) .neqv. .true.)          	error stop 133
          if (flag_values(4) .neqv. .false.)          	error stop 143
          if (flag_values(5) .neqv. .true.)          	error stop 153

        end subroutine !** end ext_sub103()
!* ---------------------------------------------------------------------

!* In ext_sub104, the halting flags that were true in ext_sub103 are
!* not modified; and the remaining flags are set to true.
        subroutine ext_sub104()
          use ieee_arithmetic
          logical*4 flag_values(5)

          call ieee_set_halting_mode(ieee_divide_by_zero, .true.)
          call ieee_set_halting_mode(ieee_underflow, .true.)

          call ieee_get_halting_mode(ieee_all, flag_values)
          if (flag_values(1) .neqv. .false.)            error stop 114
          if (flag_values(2) .neqv. .true.)             error stop 124
          if (flag_values(3) .neqv. .false.)            error stop 134
          if (flag_values(4) .neqv. .true.)             error stop 144
          if (flag_values(5) .neqv. .false.)            error stop 154

        end subroutine !** end ext_sub104()

!=======================================================================
