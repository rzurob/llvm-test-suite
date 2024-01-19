!*********************************************************************
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March 30, 2002
!*
!*  PRIMARY FUNCTIONS TESTED   : ieee_get_flag()
!*				 ieee_set_flag()
!*
!*  REFERENCE                  : Feature 180920
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : This is a FPSCR testcase.
!*				 In this testcase, main program will call
!*				 internal/external subroutines and these
!*				 subroutines will call other
!*                               internal/external subroutines.
!*.
!*				 It tests the following scenarios:
!*	1) Processes that use IEEE calling processes that don't use IEEE.
!*	2) Processes that use IEEE calling processes that use IEEE.
!*	3) Flags that cleared on entry to a process that use IEEE, and are restored on exit.
!*      4) Flags that are set in a process that use IEEE remain set on exit.
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

	program fpscrflg05
	  use ieee_arithmetic
	  implicit none

          interface


            subroutine ext_sub13()
                use ieee_arithmetic
            end subroutine

            subroutine ext_sub15()
                use ieee_arithmetic
            end subroutine

            subroutine ext_sub20()
                use xlf_fp_util
            end subroutine

          end interface

	  logical*4 flag_values(5)

!*  Check if initially all flags are false.
	  call ieee_get_flag(ieee_all, flag_values)
  	  if (any(flag_values .neqv. .false.))		error stop 1

!***********************************************************************
!*  Main program uses IEEE, and calls subroutine that uses IEEE
!*  which itself calls subroutrine that uses IEEE.
!* --------------------------------------------------------------
!*  Call to a subroutine with exception flags CLEAR on
!*  entry into the subroutine. The flags can be set on exit.
!***********************************************************************
!*** sub1: flag was not set in subroutine
          call ext_sub1()
          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .false.))           error stop 2

!*** sub4: flag was set in subroutine
          call ext_sub3()
          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .true.))           error stop 3

	  call ieee_set_flag(ieee_all, .false.)  ! clear flag
          call ieee_get_flag(ieee_all, flag_values)

!*** sub5: flag was set in subroutine
          call ext_sub5()
          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .true.))           error stop 4

	  call ieee_set_flag(ieee_all, .false.)  ! clear flag
          call ieee_get_flag(ieee_all, flag_values)

!***********************************************************************
!*  Main program uses IEEE, and calls subroutine that uses IEEE
!*  which itself calls subroutrine that does not use IEEE.
!* --------------------------------------------------------------
!*  Call to a subroutine with exception flags CLEAR on
!*  entry into the subroutine. The flags can be set on exit.
!***********************************************************************
!*** sub11: flag was not set in subroutine
          call ext_sub11()
          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .false.))           error stop 7

!*** sub13: flag was set in subroutine
          call ext_sub13()
          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .true.))           error stop 8

	  call ieee_set_flag(ieee_all, .false.)  ! clear flag
          call ieee_get_flag(ieee_all, flag_values)

!*** sub15: flag was set in subroutine
          call ext_sub15()
          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .true.))           error stop 9

	  call ieee_set_flag(ieee_all, .false.)  ! clear flag
          call ieee_get_flag(ieee_all, flag_values)

!*** sub16: flag was set in subroutine
          call ext_sub16()
          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .true.))           error stop 10

	  call ieee_set_flag(ieee_all, .false.)  ! clear flag
          call ieee_get_flag(ieee_all, flag_values)

!*** sub17: flag was set in subroutine
          call ext_sub17()
          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .true.))           error stop 11

	  call ieee_set_flag(ieee_all, .false.)  ! clear flag
          call ieee_get_flag(ieee_all, flag_values)

!***********************************************************************
!*  Main program uses IEEE, and calls subroutine that doesn't use IEEE
!*  which itself calls subroutrine that does use IEEE.
!* --------------------------------------------------------------
!*  Call to a subroutine with exception flags CLEAR on
!*  entry into the subroutine. The flags can be set on exit.
!***********************************************************************

!*** sub20: flag was set in subroutine
          call ext_sub20()
          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .true.))           error stop 12


!***********************************************************************
!*  Main program uses IEEE, and calls subroutine that uses IEEE
!*  which itself calls subroutrine that uses IEEE.
!* --------------------------------------------------------------
!*  Call to a subroutine with exception flags SET on
!*  entry into the subroutine.
!***********************************************************************
!*** sub1: flag was not set in subroutine
          call ext_sub1()
          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .true.))           error stop 21

!*** sub4: flag was set in subroutine
          call ext_sub3()
          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .true.))           error stop 22

!*** sub5: flag was set in subroutine
          call ext_sub5()
          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .true.))           error stop 23

!***********************************************************************
!*  Main program uses IEEE, and calls subroutine that uses IEEE
!*  which itself calls subroutrine that does not use IEEE.
!* --------------------------------------------------------------
!*  Call to a subroutine with exception flags SET on
!*  entry into the subroutine.
!***********************************************************************
!*** sub11: flag was not set in subroutine
          call ext_sub11()
          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .true.))           error stop 27

!*** sub13: flag was set in subroutine
          call ext_sub13()
          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .true.))           error stop 28

!*** sub15: flag was set in subroutine
          call ext_sub15()
          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .true.))           error stop 29

!*** sub16: flag was set in subroutine
          call ext_sub16()
          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .true.))           error stop 30

!*** sub17: flag was set in subroutine
          call ext_sub17()
          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .true.))           error stop 31

!***********************************************************************
!*  Main program uses IEEE, and calls subroutine that doesn't use IEEE
!*  which itself calls subroutrine that does use IEEE.
!* --------------------------------------------------------------
!*  Call to a subroutine with exception flags SET on
!*  entry into the subroutine.
!***********************************************************************

!*** sub21: flag was not changed in subroutine
          call ext_sub21()
          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .true.))           error stop 32

!=======================================================================
	contains

!***********************************************************************
!*  Rule:
!*	If there is an exception flag clear on entry into a procedure that
!*      uses IEEE intrinsic modules, the flag can be set on exit.
!***********************************************************************

!***  Sub1 calls sub2  ***
        subroutine ext_sub1()
          use ieee_arithmetic
	  logical*4 flag_values(5)

	  interface
		subroutine ext_sub2()
			use ieee_exceptions
		end subroutine
	  end interface

	  call ieee_get_flag(ieee_all, flag_values)
!*  Check if all flags are cleared when entering procedure.
          if (any(flag_values .neqv. .false.))           error stop 201

	  call ext_sub2()
	  call ieee_get_flag(ieee_all, flag_values)
!*  Check if all flags are still cleared when exiting procedure.
          if (any(flag_values .neqv. .false.))           error stop 202

	end subroutine !!ext_sub1()

!=======================================================================

!***  Sub3 calls sub4 which sets the flag  ***
        subroutine ext_sub3()
          use ieee_arithmetic
	  logical*4 flag_values(5)

	  interface
		subroutine ext_sub4()
			use ieee_arithmetic
		end subroutine
	  end interface

	  call ieee_get_flag(ieee_all, flag_values)
!*  Check if all flags are cleared when entering procedure.
          if (any(flag_values .neqv. .false.))           error stop 204

	  call ext_sub4()
	  call ieee_get_flag(ieee_all, flag_values)
!*  Check if all flags are set when exiting procedure.
          if (any(flag_values .neqv. .true.))           error stop 205

	end subroutine !!ext_sub3()

!=======================================================================

!***  Sub5 calls sub4 which sets the flag  ***
!*  sub4 has no interface inside sub5
        subroutine ext_sub5()
          use ieee_arithmetic
	  logical*4 flag_values(5)

	  call ieee_get_flag(ieee_all, flag_values)
!*  Check if all flags are cleared when entering procedure.
          if (any(flag_values .neqv. .false.))           error stop 206

	  call ext_sub4()
	  call ieee_get_flag(ieee_all, flag_values)
!*  Check if all flags are set when exiting procedure.
          if (any(flag_values .neqv. .true.))           error stop 207

	end subroutine !!ext_sub5()

!=======================================================================

	end program
!=======================================================================

!***  Sub2:  The flag is not modified in the subroutine
!***  Sub2 is called by other subroutines
        subroutine ext_sub2()
          use ieee_exceptions
          logical*4 flag_values(5)

          call ieee_get_flag(ieee_all, flag_values)
!*  Check if all flags are cleared when entering procedure.
          if (any(flag_values .neqv. .false.))           error stop 203
        end subroutine !!ext_sub2()
!=======================================================================

!***  Sub4:  The flag is set inside the subroutine
!***  Sub4 is called by other subroutines
        subroutine ext_sub4()
          use ieee_arithmetic
          logical*4 flag_values(5)


!*  Check if all flags are cleared when entering procedure.
          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .false.))           error stop 213

!*  Assign true to all exception flags.
          call ieee_set_flag(ieee_all, .true.)

          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .true.))           error stop 217

        end subroutine !!ext_sub4()
!=======================================================================


!-----------------------------------------------------------------------
!***********************************************************************
!*  External subroutine with exception flags set on entry
!*  calls another subroutine that does not
!*  use IEEE modules.
!***********************************************************************
!***  sub11 calls sub12  ***
        subroutine ext_sub11()
          use ieee_arithmetic
	  logical*4 flag_values(5)

	  interface
		subroutine ext_sub12()
			use xlf_fp_util
		end subroutine
	  end interface

	  call ieee_get_flag(ieee_all, flag_values)
!*  Check if all flags are cleared when entering procedure.
          if (any(flag_values .neqv. .false.))           error stop 301

	  call ext_sub12()
	  call ieee_get_flag(ieee_all, flag_values)
!*  Check if all flags are still cleared when exiting procedure.
          if (any(flag_values .neqv. .false.))           error stop 302

	end subroutine !!ext_sub11()
!-----------------------------------------------------------------------

        subroutine ext_sub12()
          use xlf_fp_util

          integer(fpscr_kind) :: flag_values(5)

          flag_values(1) = get_fpscr_flags(fp_overflow)
          flag_values(2) = get_fpscr_flags(fp_div_by_zero)
          flag_values(3) = get_fpscr_flags(fp_invalid)
          flag_values(4) = get_fpscr_flags(fp_underflow)
          flag_values(5) = get_fpscr_flags(fp_inexact)

!*  Check if all flags are initially false.
          if (any(flag_values .ne. 0))           	error stop 303

        end subroutine !!ext_sub12()
!=======================================================================

!***  Sub13 calls sub14 which sets the flag  ***
        subroutine ext_sub13()
          use ieee_arithmetic
	  logical*4 flag_values(5)

	  interface
		subroutine ext_sub14()
			use xlf_fp_util
		end subroutine
	  end interface

	  call ieee_get_flag(ieee_all, flag_values)
!*  Check if all flags are cleared when entering procedure.
          if (any(flag_values .neqv. .false.))           error stop 304

	  call ext_sub14()
	  call ieee_get_flag(ieee_all, flag_values)
!*  Check if all flags are set when exiting procedure.
          if (any(flag_values .neqv. .true.))           error stop 305

	end subroutine !!ext_sub13()

!=======================================================================

!***  Sub15 calls sub14 which sets the flag  ***
!*  sub14 has no interface inside sub15
        subroutine ext_sub15()
          use ieee_arithmetic
	  logical*4 flag_values(5)

	  call ieee_get_flag(ieee_all, flag_values)
!*  Check if all flags are cleared when entering procedure.
          if (any(flag_values .neqv. .false.))           error stop 306

	  call ext_sub14()
	  call ieee_get_flag(ieee_all, flag_values)
!*  Check if all flags are set when exiting procedure.
          if (any(flag_values .neqv. .true.))           error stop 307

	end subroutine !!ext_sub15()

!=======================================================================

!***  Sub16 calls sub14 which sets the flag  ***
!*  sub16 has no interface inside main program
        subroutine ext_sub16()
          use ieee_arithmetic
	  logical*4 flag_values(5)

	  interface
		subroutine ext_sub14()
			use xlf_fp_util
		end subroutine
	  end interface

	  call ieee_get_flag(ieee_all, flag_values)
!*  Check if all flags are cleared when entering procedure.
          if (any(flag_values .neqv. .false.))           error stop 308

	  call ext_sub14()
	  call ieee_get_flag(ieee_all, flag_values)
!*  Check if all flags are set when exiting procedure.
          if (any(flag_values .neqv. .true.))           error stop 309

	end subroutine !!ext_sub16()

!=======================================================================

!***  Sub17 calls sub14 which sets the flag  ***
!*  sub17 has no interface inside main program
!*  sub14 has no interface inside sub17

        subroutine ext_sub17()
          use ieee_arithmetic
	  logical*4 flag_values(5)

	  call ieee_get_flag(ieee_all, flag_values)
!*  Check if all flags are cleared when entering procedure.
          if (any(flag_values .neqv. .false.))           error stop 310

	  call ext_sub14()
	  call ieee_get_flag(ieee_all, flag_values)
!*  Check if all flags are set when exiting procedure.
          if (any(flag_values .neqv. .true.))           error stop 311

	end subroutine !!ext_sub17()
!-----------------------------------------------------------------------

!***  Sub14:  The flag is set inside the subroutine
        subroutine ext_sub14()
          use xlf_fp_util

          integer(fpscr_kind) :: flag_values(5)

          flag_values(1) = get_fpscr_flags(fp_overflow)
          flag_values(2) = get_fpscr_flags(fp_div_by_zero)
          flag_values(3) = get_fpscr_flags(fp_invalid)
          flag_values(4) = get_fpscr_flags(fp_underflow)
          flag_values(5) = get_fpscr_flags(fp_inexact)

!*  Check if all flags are initially false.
          if (any(flag_values .ne. 0))           	error stop 312

!*  Assign true to all flags.
          call set_fpscr_flags(fp_overflow)
          call set_fpscr_flags(fp_div_by_zero)
          call set_fpscr_flags(fp_inv_isi)
          call set_fpscr_flags(fp_underflow)
          call set_fpscr_flags(fp_inexact)

          flag_values(1) = get_fpscr_flags(fp_overflow)
          flag_values(2) = get_fpscr_flags(fp_div_by_zero)
          flag_values(3) = get_fpscr_flags(fp_invalid)
          flag_values(4) = get_fpscr_flags(fp_underflow)
          flag_values(5) = get_fpscr_flags(fp_inexact)

!*  Check if all flags are set to true.
          if (any(flag_values .eq. 0))          	 error stop 313

        end subroutine !!ext_sub14()


!-----------------------------------------------------------------------
!***********************************************************************
!*  External subroutine that doesn't use IEEE calls subroutine
!*  that uses IEEE.
!*  Flag is CLEAR in main program.
!***********************************************************************
!***  sub20 calls sub4  ***
        subroutine ext_sub20()
          use xlf_fp_util
          integer(fpscr_kind) :: flag_values(5)

	  interface
		subroutine ext_sub4()
			use ieee_exceptions
		end subroutine
	  end interface

          flag_values(1) = get_fpscr_flags(fp_overflow)
          flag_values(2) = get_fpscr_flags(fp_div_by_zero)
          flag_values(3) = get_fpscr_flags(fp_invalid)
          flag_values(4) = get_fpscr_flags(fp_underflow)
          flag_values(5) = get_fpscr_flags(fp_inexact)

!*  Check if all flags are initially false.
          if (any(flag_values .ne. 0))           	error stop 314

	  call ext_sub4()

          flag_values(1) = get_fpscr_flags(fp_overflow)
          flag_values(2) = get_fpscr_flags(fp_div_by_zero)
          flag_values(3) = get_fpscr_flags(fp_invalid)
          flag_values(4) = get_fpscr_flags(fp_underflow)
          flag_values(5) = get_fpscr_flags(fp_inexact)

!*  Check if all flags are set to true.
          if (any(flag_values .eq. 0))          	 error stop 315

        end subroutine !!ext_sub20()

!-----------------------------------------------------------------------
!***********************************************************************
!*  External subroutine that doesn't use IEEE calls subroutine
!*  that uses IEEE.
!*  Flag is SET in main program.
!***********************************************************************
!***  sub21 calls sub4  ***
        subroutine ext_sub21()
          use xlf_fp_util
          integer(fpscr_kind) :: flag_values(5)

	  interface
		subroutine ext_sub4()
			use ieee_exceptions
		end subroutine
	  end interface

          flag_values(1) = get_fpscr_flags(fp_overflow)
          flag_values(2) = get_fpscr_flags(fp_div_by_zero)
          flag_values(3) = get_fpscr_flags(fp_invalid)
          flag_values(4) = get_fpscr_flags(fp_underflow)
          flag_values(5) = get_fpscr_flags(fp_inexact)

!*  Check if all flags are initially true, since its set in main program.
          if (any(flag_values .eq. 0))           	error stop 316

	  call ext_sub4()

          flag_values(1) = get_fpscr_flags(fp_overflow)
          flag_values(2) = get_fpscr_flags(fp_div_by_zero)
          flag_values(3) = get_fpscr_flags(fp_invalid)
          flag_values(4) = get_fpscr_flags(fp_underflow)
          flag_values(5) = get_fpscr_flags(fp_inexact)

!*  Check if all flags are still set to true.
          if (any(flag_values .eq. 0))          	 error stop 317

        end subroutine !!ext_sub21()

!=======================================================================
