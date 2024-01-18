!*********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90 -qstrict -qfloat=nofold -qnoipa
! %GROUP: fpscrflg_dp.f
! %VERIFY:
! %STDIN:
! %STDOUT: fpscrflg_dp.out
! %EXECARGS:
! %POSTCMD: rm -f fpscrflg_dp.out
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March 30, 2002
!*
!*  PRIMARY FUNCTIONS TESTED   : ieee_get_flag(),
!*				 ieee_set_flag(),
!*				 ieee_overflow,
!*				 ieee_divide_by_zero,
!*				 ieee_invalid,
!*				 ieee_underflow,
!*				 ieee_inexact,
!*				 get_fpscr_flags(),
!*				 set_fpscr_flags(),
!*				 fp_overflow,
!*				 fp_div_by_zero,
!*				 fp_underflow,
!*				 fp_inexact,
!*				 fp_invalid,
!*				 fp_inv_isi,
!*				 fp_inv_idi
!*
!*  REFERENCE                  : Feature 180920
!*
!*  REQUIRED COMPILER OPTIONS  : -qstrict -qfloat=nofold -qnoipa
!*
!*  DESCRIPTION                : This is a FPSCR testcase.
!*				 This testcase uses double precision variables.
!*				 The results should be the same as for REAL*8
!*				 variables (fpscrflg_r8.f).
!*
!*				 In this testcase, main program that
!*				 uses IEEE modules will
!*                               call internal and external subroutines
!*                               that use or don't use IEEE modules.
!*
!*				 It tests the following
!*				 scenarios:
!*	1) Processes that use IEEE calling processes that don't use IEEE.
!*	2) Processes that use IEEE calling processes that use IEEE.
!*	3) Flags that cleared on entry to a process that use IEEE, and are restored on exit.
!*      4) Flags that are set in a process that use IEEE remain set on exit.
!*
!*	This testcase will also make sure that ieee_arithmetic is
!*      in fact the super-set of ieee_exceptions by using ieee_arithmetic
!*	in one subroutine and then using ieee_exceptions in the next, and so on.
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

	program fpscrdp
	  use ieee_arithmetic
	  implicit none

          interface

!*** external subroutine that uses ieee_arithmetic ***
            subroutine ext_sub1()
	  	use ieee_arithmetic
            end subroutine

!*** external subroutine that uses ieee_exceptions ***
            subroutine ext_sub2()
	  	use ieee_exceptions
            end subroutine

!*** external subroutine that uses ieee_arithmetic ***
            subroutine ext_sub4()
                use ieee_arithmetic
            end subroutine

!*** external subroutine that uses ieee_exceptions ***
            subroutine ext_sub5()
                use ieee_exceptions
            end subroutine

!*** external subroutine that uses ieee_arithmetic ***
            subroutine ext_sub7()
                use ieee_arithmetic
            end subroutine

!*** external subroutine that uses ieee_exceptions ***
            subroutine ext_sub8()
                use ieee_exceptions
            end subroutine

!*** external subroutine that doesn't uses ieee_exceptions and uses xlf_fp_util ***
            subroutine ext_sub13()
                use xlf_fp_util
            end subroutine

!*** external subroutine that doesn't uses ieee_exceptions and uses xlf_fp_util ***
            subroutine ext_sub14()
                use xlf_fp_util
            end subroutine

!*** external subroutine that doesn't uses ieee_exceptions and uses xlf_fp_util ***
            subroutine ext_sub17()
                use xlf_fp_util
            end subroutine

!*** external subroutine that doesn't uses ieee_exceptions and uses xlf_fp_util ***
            subroutine ext_sub18()
		use xlf_fp_util
            end subroutine

!*** external subroutine that doesn't uses ieee_exceptions and uses xlf_fp_util ***
            subroutine ext_sub19()
                use xlf_fp_util
            end subroutine

          end interface

	  logical*4 flag_values(5)

	  integer exp8
	  double precision zero8

	  double precision tmpr8
	  double precision overflow_r8
	  double precision divbyzero_r8
	  double precision underflow_r8

!*  Check if initially all flags are false.
	  call ieee_get_flag(ieee_all, flag_values)
  	  if (any(flag_values .neqv. .false.))		error stop 1

!***********************************************************************
!*  Process that use IEEE calling processes that use IEEE.
!* --------------------------------------------------------------
!*  Call to an external subroutine with exception flags CLEAR on
!*  entry into the subroutine. The flags can be set on exit.
!***********************************************************************
!*** sub1: flag was not set in subroutine
          call ext_sub1()
          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .false.))           error stop 3

!*** sub2: flag was not set in subroutine
          call ext_sub2()
          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .false.))           error stop 4

!*** sub4: flag was set in subroutine
          call ext_sub4()
          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .true.))           error stop 5

	  call ieee_set_flag(ieee_all, .false.)  ! clear flag
          call ieee_get_flag(ieee_all, flag_values)

!*** sub5: flag was set in subroutine
          call ext_sub5()
          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .true.))           error stop 6

	  call ieee_set_flag(ieee_all, .false.)  ! clear flag
          call ieee_get_flag(ieee_all, flag_values)

!*** sub7: flag was set then cleared in subroutine
          call ext_sub7()
          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .false.))           error stop 7

!*** sub8: flag was set then cleared in subroutine
          call ext_sub8()
          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .false.))           error stop 8

!*** sub9: flag was set in subroutine with no interface
          call ext_sub9()
          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .true.))           error stop 9

	  call ieee_set_flag(ieee_all, .false.) ! clear flag
          call ieee_get_flag(ieee_all, flag_values)

!*** sub10: flag was set in subroutine with no interface
          call ext_sub10()
          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .true.))           error stop 10

	  call ieee_set_flag(ieee_all, .false.)  ! clear flag
          call ieee_get_flag(ieee_all, flag_values)

!*** sub11: flag was set and then cleared in subroutine with no interface
          call ext_sub11()
          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .false.))           error stop 11

!*** sub12: flag was set and then cleared in subroutine with no interface
          call ext_sub12()
          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .false.))           error stop 12


!***********************************************************************
!*  Assign true to all exception flags.
!***********************************************************************
          tmpr8 = 2.0d0
	  zero8	= 0.0d0
          overflow_r8 		= huge(tmpr8)**(1023)
	  divbyzero_r8 		= 10./zero8
          underflow_r8 		= tiny(tmpr8)/huge(tmpr8)

!*** IPA optimizes invalid calculations too well, so force ieee_invalid to TRUE
	  call ieee_set_flag(ieee_invalid, .true.)


!***********************************************************************
!*  The if loops below are to ensure that variables: overflow_r8,
!*  divbyzero_r8, invalid_r8, underflow_r8 affect the output of the program.
!*  If a variable doesn't affect the output, TOBEY throws away the variable
!*  - we don't want that to happen in this situation.
!***********************************************************************
	  if (overflow_r8 .le.(2**1023))		error stop 101
	  if (divbyzero_r8 .le.(2**1023))		error stop 102
	  if (underflow_r8 .gt.(2**(-1074)))		error stop 104

          call ieee_get_flag(ieee_all, flag_values)
  	  if (any(flag_values .neqv. .true.))		error stop 105

!***********************************************************************
!*  Process that use IEEE calling processes that use IEEE.
!* --------------------------------------------------------------
!*  Call to an external subroutine with exception flags SET on entry
!*  into the subroutine.
!***********************************************************************
!***  On entry, the flag is cleared, and on exit it is reset to true.

!*** sub1: flag was not changed explicitely in subroutine
	  call ext_sub1()
	  call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .true.))           error stop 31

!*** sub2: flag was not changed explicitely in subroutine
          call ext_sub2()
          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .true.))           error stop 32

!*** sub4: flag was set in subroutine
          call ext_sub4()
          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .true.))           error stop 33

!*** sub5: flag was set in subroutine
          call ext_sub5()
          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .true.))           error stop 34

!*** sub7: flag was set then cleared in subroutine; flag can not be cleared on exit
          call ext_sub7()
          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .true.))           error stop 35

!*** sub7: flag was set then cleared in subroutine; flag can not be cleared on exit
          call ext_sub8()
          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .true.))           error stop 36

!*** sub9: flag was set in subroutine with no interface
          call ext_sub9()
          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .true.))           error stop 37

!*** sub10: flag was set in subroutine with no interface
          call ext_sub10()
          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .true.))           error stop 38

!*** sub11: flag was set and then cleared in subroutine with no interface; flag can not be cleared
          call ext_sub11()
          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .true.))           error stop 39

!*** sub12: flag was set and then cleared in subroutine with no interface; flag can not be cleared
          call ext_sub12()
          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .true.))           error stop 40


!***********************************************************************
!*  Process that use IEEE calling processes that don't use IEEE.
!* --------------------------------------------------------------
!*  Call to an external subroutine with exception flags CLEAR on entry
!*  into the subroutine.
!***********************************************************************
	  call ieee_set_flag(ieee_all, .false.)  ! clear flag
	  call ieee_get_flag(ieee_all, flag_values)

!*** This subroutine doesn't use ieee modules.
     	  call ext_sub13()
          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .false.))           error stop 51

!*** This subroutine doesn't use ieee modules and sets flag.
     	  call ext_sub14()
          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .true.))           error stop 52

	  call ieee_set_flag(ieee_all, .false.)  ! clear flag
	  call ieee_get_flag(ieee_all, flag_values)

!*** This subroutine doesn't use ieee modules wtih no interface and sets flag by calculation.
     	  call ext_sub15()
          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .true.))  		error stop 53

	  call ieee_set_flag(ieee_all, .false.)  ! clear flag
	  call ieee_get_flag(ieee_all, flag_values)

!*** This subroutine doesn't use ieee modules wtih no interface and sets flag by set_fpscr_flags.
     	  call ext_sub16()
          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .true.))  		error stop 54

	  call ieee_set_flag(ieee_all, .false.)  ! clear flag
	  call ieee_get_flag(ieee_all, flag_values)

!*** This subroutine doesn't use ieee modules and sets flag, and tries to clear it by calculation.
          call ext_sub17()
          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .true.))           error stop 55

	  call ieee_set_flag(ieee_all, .false.)  ! clear flag
	  call ieee_get_flag(ieee_all, flag_values)

!*** This subroutine doesn't use ieee modules and sets flag, and clears it by clr_fpscr_flags.
          call ext_sub18()
          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .false.))           error stop 56


!***********************************************************************
!*  Process that use IEEE calling processes that don't use IEEE.
!* --------------------------------------------------------------
!*  Call to an external subroutine with exception flags SET on entry
!*  into the subroutine.
!***********************************************************************

          call ieee_set_flag(ieee_all, .true.)  ! set flag
          call ieee_get_flag(ieee_all, flag_values)

!*** This subroutine doesn't use ieee modules.
          call ext_sub19()
          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .true.))           error stop 71

!*** This subroutine doesn't use ieee modules and sets flag, and tries to clear it by calculation.
!*** Flag cannot be cleared
          call ext_sub20()
          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .true.))           error stop 72

!*** This subroutine doesn't use ieee modules and sets flag, and clears it by clr_fpscr_flags.
!*** Flag cannot be cleared
          call ext_sub21()
          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .true.))           error stop 73

          call ieee_set_flag(ieee_all, .false.)	! clear flags
          call ieee_get_flag(ieee_all, flag_values)


!***********************************************************************
!*  Internal subroutines
!***********************************************************************
!***  Internal subroutine with flags CLEAR on entry and exit.
          call int_sub1()
          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .false.))           error stop 81

!***  Internal subroutine with flags clear on entry and set on exit.
          call int_sub2()
          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .true.))           error stop 82

          call ieee_set_flag(ieee_all, .false.)	! clear flags
          call ieee_get_flag(ieee_all, flag_values)

!***  Internal subroutine with flags clear on entry and  set inside
!***  the subroutine and then cleared on exit.
          call int_sub4()
          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .false.))           error stop 83

!***  Flag set on entry for internal subroutines
          call ieee_set_flag(ieee_all, .true.)	! set flags
          call ieee_get_flag(ieee_all, flag_values)

!***  Internal subroutine with flags set on entry and exit.
          call int_sub1()
          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .true.))           error stop 84

!***  Internal subroutine with flags set on entry and exit.
          call int_sub2()
          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .true.))           error stop 85

!***  Internal subroutine with flags SET on entry and CLEARED on exit.
!***  Cannot clear a flag on exit.
          call int_sub4()
          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .true.))           error stop 86


	  contains

!***  Internal subroutine with flags CLEAR on entry and exit.
          subroutine int_sub1()

	    call ieee_get_flag(ieee_all, flag_values)
!*  Check if all flags are not changed entering procedure.
            if (any(flag_values .neqv. .false.))           error stop 401

	  end subroutine !!int_sub1()
!=======================================================================

!***  Internal subroutine with flags SET inside subroutine and stay SET on exit.
            subroutine int_sub2()

              call ieee_set_flag(ieee_all, .true.)
              call ieee_get_flag(ieee_all, flag_values)
!*  Check if all flags are set; the flags should be set when exiting the procedure.
              if (any(flag_values .neqv. .true.))           error stop 402

            end subroutine !!int_sub2()
!=======================================================================

!***  Internal subroutine with flags SET then CLEARED inside subroutine
            subroutine int_sub4()

              call ieee_set_flag(ieee_all, .true.)
              call ieee_get_flag(ieee_all, flag_values)
!*  Check if all flags are set
              if (any(flag_values .neqv. .true.))           error stop 404

!&  Clear all flags.
              call ieee_set_flag(ieee_all, .false.)
              call ieee_get_flag(ieee_all, flag_values)
!*  Check if all flags are cleared; flags should stay clear on exit.
              if (any(flag_values .neqv. .false.))           error stop 405

            end subroutine !!int_sub4()
!=======================================================================

	end program

!=======================================================================

!***********************************************************************
!*  Rule:
!*	If there is an exception flag set on entry into a procedure that
!*      uses IEEE intrinsic modules, the flag clears on entry into the
!* 	procedure and resets when returning from the procedure.
!***********************************************************************
        subroutine ext_sub1()
          use ieee_arithmetic
	  logical*4 flag_values(5)
	  call ieee_get_flag(ieee_all, flag_values)
!*  Check if all flags are cleared when entering procedure.
         if (any(flag_values .neqv. .false.))           error stop 201
	end subroutine !!ext_sub1()
!=======================================================================

        subroutine ext_sub2()
          use ieee_exceptions
	  logical*4 flag_values(5)
          call ieee_get_flag(ieee_all, flag_values)
!*  Check if all flags are cleared when entering procedure.
          if (any(flag_values .neqv. .false.))           error stop 202
        end subroutine !!ext_sub2()
!=======================================================================


!*  The flag is set inside the subroutine and then cleared to make sure
!*  that this can be done without problems.
        subroutine ext_sub4()
          use ieee_arithmetic
          logical*4 flag_values(5)

	  integer exp8
	  double precision zero8

	  double precision tmpr8
	  double precision overflow_r8
	  double precision divbyzero_r8
	  double precision underflow_r8

!*  Check if all flags are cleared when entering procedure.
          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .false.))           error stop 203

!*  Assign true to all exception flags.
          tmpr8 = 2.0d0
	  zero8	= 0.0d0
          overflow_r8 		= huge(tmpr8)**(1023)
	  divbyzero_r8 		= 10./zero8
          underflow_r8 		= tiny(tmpr8)/huge(tmpr8)

!*** IPA optimizes invalid calculations too well, so force ieee_invalid to TRUE
	  call ieee_set_flag(ieee_invalid, .true.)


!***********************************************************************
!*  The if loops below are to ensure that variables: overflow_r8,
!*  divbyzero_r8, invalid_r8, underflow_r8 affect the output of the program.
!*  If a variable doesn't affect the output, TOBEY throws away the variable
!*  - we don't want that to happen in this situation.
!***********************************************************************
	  if (overflow_r8 .le.(2**1023))		error stop 204
	  if (divbyzero_r8 .le.(2**1023))		error stop 205
	  if (underflow_r8 .gt.(2**(-1074)))		error stop 206

          call ieee_get_flag(ieee_all, flag_values)
  	  if (any(flag_values .neqv. .true.))		error stop 207

        end subroutine !!ext_sub4()
!=======================================================================

!*  The flag is set inside the subroutine and then cleared to make sure
!*  that this can be done without problems.
        subroutine ext_sub5()
          use ieee_exceptions
          logical*4 flag_values(5)

	  integer exp8
	  double precision zero8

	  double precision tmpr8
	  double precision overflow_r8
	  double precision divbyzero_r8
	  double precision underflow_r8

!*  Check if all flags are cleared when entering procedure.
          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .false.))           error stop 208

!*  Assign true to all exception flags.
          tmpr8 = 2.0d0
	  zero8	= 0.0d0
          overflow_r8 		= huge(tmpr8)**(1023)
	  divbyzero_r8 		= 10./zero8
          underflow_r8 		= tiny(tmpr8)/huge(tmpr8)

!*** IPA optimizes invalid calculations too well, so force ieee_invalid to TRUE
	  call ieee_set_flag(ieee_invalid, .true.)

	  if (overflow_r8 .le.(2**1023))		error stop 209
	  if (divbyzero_r8 .le.(2**1023))		error stop 210
	  if (underflow_r8 .gt.(2**(-1074)))		error stop 211

          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .true.))           error stop 212

        end subroutine !!ext_sub5()
!=======================================================================


!-----------------------------------------------------------------------
!***********************************************************************
!*  External subroutine with exception flags set on entry
!*  into the subroutine. This subroutine uses ieee_arithmetic and
!*  has an interface inside main with ieee_arithmetic in the interface.
!*
!*  The flag is set inside the subroutine and then cleared to make sure
!*  that this can be done without problems.
!*  ----------------------------------------------------------------
!*  Rule:
!*      If there is an exception flag set on entry into a procedure that
!*      uses IEEE intrinsic modules, the flag clears on entry into the
!*      procedure and resets when returning from the procedure.
!***********************************************************************
        subroutine ext_sub7()
          use ieee_arithmetic
          logical*4 flag_values(5)
	  integer exp8
	  double precision zero8

	  double precision tmpr8
	  double precision overflow_r8
	  double precision divbyzero_r8
	  double precision underflow_r8

!*  Check if all flags are cleared when entering procedure.
          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .false.))           error stop 213

!*  Assign true to all exception flags.
          tmpr8 = 2.0d0
	  zero8	= 0.0d0
          overflow_r8 		= huge(tmpr8)**(1023)
	  divbyzero_r8 		= 10./zero8
          underflow_r8 		= tiny(tmpr8)/huge(tmpr8)

!*** IPA optimizes invalid calculations too well, so force ieee_invalid to TRUE
	  call ieee_set_flag(ieee_invalid, .true.)


!***********************************************************************
!*  The four if loops below are to ensure that variables: overflow_r8,
!*  divbyzero_r8, invalid_r8, underflow_r8 affect the output of the program.
!*  If a variable doesn't affect the output, TOBEY throws away the variable
!*  - we don't want that to happen in this situation.
!***********************************************************************
	  if (overflow_r8 .le.(2**1023))		error stop 214
	  if (divbyzero_r8 .le.(2**1023))		error stop 215
	  if (underflow_r8 .gt.(2**(-1074)))		error stop 216

          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .true.))           error stop 217

          call ieee_set_flag(ieee_all, .false.)
          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .false.))          error stop 218

        end subroutine !!ext_sub7()
!=======================================================================

!*  The flag is set inside the subroutine and then cleared to make sure
!*  that this can be done without problems.
        subroutine ext_sub8()
          use ieee_exceptions
          logical*4 flag_values(5)
	  integer exp8
	  double precision zero8

	  double precision tmpr8
	  double precision overflow_r8
	  double precision divbyzero_r8
	  double precision underflow_r8

!*  Check if all flags are cleared when entering procedure.
          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .false.))           error stop 219

          tmpr8 = 2.0d0
	  zero8	= 0.0d0
          overflow_r8 		= huge(tmpr8)**(1023)
	  divbyzero_r8 		= 10./zero8
          underflow_r8 		= tiny(tmpr8)/huge(tmpr8)

!*** IPA optimizes invalid calculations too well, so force ieee_invalid to TRUE
	  call ieee_set_flag(ieee_invalid, .true.)


!***********************************************************************
!*  The four if loops below are to ensure that variables: overflow_r8,
!*  divbyzero_r8, invalid_r8, underflow_r8 affect the output of the program.
!*  If a variable doesn't affect the output, TOBEY throws away the variable
!*  - we don't want that to happen in this situation.
!***********************************************************************
	  if (overflow_r8 .le.(2**1023))		error stop 220
	  if (divbyzero_r8 .le.(2**1023))		error stop 221
	  if (underflow_r8 .gt.(2**(-1074)))		error stop 222

          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .true.))           error stop 223

          call ieee_set_flag(ieee_all, .false.)
          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .false.))          error stop 224

        end subroutine !!ext_sub8()
!=======================================================================

!***********************************************************************
!*  External subroutine with exception flags set on entry
!*  into the subroutine. This subroutine uses ieee_arithmetic and
!*  NO interface.
!*  The flag is set inside the subroutine and then cleared to make sure
!*  that this can be done without problems.
!*  ----------------------------------------------------------------
!*  Rule:
!*      If there is an exception flag set on entry into a procedure that
!*      uses IEEE intrinsic modules, the flag clears on entry into the
!*      procedure and resets when returning from the procedure.
!***********************************************************************
        subroutine ext_sub9()
          use ieee_arithmetic
          logical*4 flag_values(5)
	  integer exp8
	  double precision zero8

	  double precision tmpr8
	  double precision overflow_r8
	  double precision divbyzero_r8
	  double precision underflow_r8

!*  Check if all flags are cleared when entering procedure.
          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .false.))           error stop 225

!*  Assign true to all exception flags.
          tmpr8 = 2.0d0
	  zero8	= 0.0d0
          overflow_r8 		= huge(tmpr8)**(1023)
	  divbyzero_r8 		= 10./zero8
          underflow_r8 		= tiny(tmpr8)/huge(tmpr8)

!*** IPA optimizes invalid calculations too well, so force ieee_invalid to TRUE
	  call ieee_set_flag(ieee_invalid, .true.)

	  if (overflow_r8 .le.(2**1023))		error stop 226
	  if (divbyzero_r8 .le.(2**1023))		error stop 227
	  if (underflow_r8 .gt.(2**(-1074)))		error stop 228

          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .true.))           error stop 229

        end subroutine !!ext_sub9()
!=======================================================================

        subroutine ext_sub10()
          use ieee_exceptions
          logical*4 flag_values(5)
	  integer exp8
	  double precision zero8

	  double precision tmpr8
	  double precision overflow_r8
	  double precision divbyzero_r8
	  double precision underflow_r8

!*  Check if all flags are cleared when entering procedure.
          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .false.))           error stop 230

!*  Assign true to all exception flags.
          tmpr8 = 2.0d0
	  zero8	= 0.0d0
          overflow_r8 		= huge(tmpr8)**(1023)
	  divbyzero_r8 		= 10./zero8
          underflow_r8 		= tiny(tmpr8)/huge(tmpr8)

!*** IPA optimizes invalid calculations too well, so force ieee_invalid to TRUE
	  call ieee_set_flag(ieee_invalid, .true.)


!***********************************************************************
!*  The four if loops below are to ensure that variables: overflow_r8,
!*  divbyzero_r8, invalid_r8, underflow_r8 affect the output of the program.
!*  If a variable doesn't affect the output, TOBEY throws away the variable
!*  - we don't want that to happen in this situation.
!***********************************************************************
	  if (overflow_r8 .le.(2**1023))		error stop 231
	  if (divbyzero_r8 .le.(2**1023))		error stop 232
	  if (underflow_r8 .gt.(2**(-1074)))		error stop 233

          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .true.))           error stop 234

        end subroutine !!ext_sub10()
!=======================================================================


!-----------------------------------------------------------------------
!***********************************************************************
!*  External subroutine with exception flags set on entry
!*  into the subroutine. This subroutine uses ieee_arithmetic and
!*  has an interface inside main with ieee_arithmetic in the interface.
!*
!*  The flag is set inside the subroutine and then cleared to make sure
!*  that this can be done without problems.
!*  ----------------------------------------------------------------
!*  Rule:
!*      If there is an exception flag set on entry into a procedure that
!*      uses IEEE intrinsic modules, the flag clears on entry into the
!*      procedure and resets when returning from the procedure.
!***********************************************************************
        subroutine ext_sub11()
          use ieee_arithmetic
          logical*4 flag_values(5)
	  integer exp8
	  double precision zero8

	  double precision tmpr8
	  double precision overflow_r8
	  double precision divbyzero_r8
	  double precision underflow_r8

!*  Check if all flags are cleared when entering procedure.
          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .false.))           error stop 235

!*  Assign true to all exception flags.
          tmpr8 = 2.0d0
	  zero8	= 0.0d0
          overflow_r8 		= huge(tmpr8)**(1023)
	  divbyzero_r8 		= 10./zero8
          underflow_r8 		= tiny(tmpr8)/huge(tmpr8)

!*** IPA optimizes invalid calculations too well, so force ieee_invalid to TRUE
	  call ieee_set_flag(ieee_invalid, .true.)

	  if (overflow_r8 .le.(2**1023))		error stop 236
	  if (divbyzero_r8 .le.(2**1023))		error stop 237
	  if (underflow_r8 .gt.(2**(-1074)))		error stop 238

          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .true.))           error stop 239

          call ieee_set_flag(ieee_all, .false.)
          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .false.))          error stop 240

        end subroutine !!ext_sub11()
!=======================================================================

        subroutine ext_sub12()
          use ieee_exceptions
          logical*4 flag_values(5)
	  integer exp8
	  double precision zero8

	  double precision tmpr8
	  double precision overflow_r8
	  double precision divbyzero_r8
	  double precision underflow_r8

!*  Check if all flags are cleared when entering procedure.
          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .false.))           error stop 241

!*  Assign true to all exception flags.
          tmpr8 = 2.0d0
	  zero8	= 0.0d0
          overflow_r8 		= huge(tmpr8)**(1023)
	  divbyzero_r8 		= 10./zero8
          underflow_r8 		= tiny(tmpr8)/huge(tmpr8)

!*** IPA optimizes invalid calculations too well, so force ieee_invalid to TRUE
	  call ieee_set_flag(ieee_invalid, .true.)

	  if (overflow_r8 .le.(2**1023))		error stop 242
	  if (divbyzero_r8 .le.(2**1023))		error stop 243
	  if (underflow_r8 .gt.(2**(-1074)))		error stop 244

          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .true.))           error stop 245

          call ieee_set_flag(ieee_all, .false.)
          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .false.))          error stop 246

        end subroutine !!ext_sub12()
!=======================================================================

!***********************************************************************
!*  External subroutine with exception flags CLEAR on entry
!*  into the subroutine. This subroutine doesn't use ieee modules
!*  ----------------------------------------------------------------
!*  Rule:
!*	Calls to procedures that do not use the ieee modules from
!*	procedures that do, will not change the floating point status
!*	except by setting exception flags.
!***********************************************************************
!*  Subroutine has an interface inside main.
        subroutine ext_sub13()
	  use xlf_fp_util
          integer(fpscr_kind) :: flag_values(5)

          flag_values(1) = get_fpscr_flags(fp_overflow)
          flag_values(2) = get_fpscr_flags(fp_div_by_zero)
          flag_values(3) = get_fpscr_flags(fp_invalid)
          flag_values(4) = get_fpscr_flags(fp_underflow)
          flag_values(5) = get_fpscr_flags(fp_inexact)

!*  Check if all flags are initially false.
          if (any(flag_values .ne. 0))           	error stop 247

        end subroutine !!ext_sub13()
!=======================================================================

!*  Subroutine has an interface inside main and sets flag.

        subroutine ext_sub14()
	  use xlf_fp_util
          integer(fpscr_kind) :: flag_values(5)
	  integer exp8
	  double precision zero8

	  double precision tmpr8
	  double precision overflow_r8
	  double precision divbyzero_r8
	  double precision underflow_r8

          flag_values(1) = get_fpscr_flags(fp_overflow)
          flag_values(2) = get_fpscr_flags(fp_div_by_zero)
          flag_values(3) = get_fpscr_flags(fp_invalid)
          flag_values(4) = get_fpscr_flags(fp_underflow)
          flag_values(5) = get_fpscr_flags(fp_inexact)

!*  Check if all flags are initially false.
          if (any(flag_values .ne. 0))           	error stop 248

!*  Assign true to all exception flags.
          tmpr8 = 2.0d0
	  zero8	= 0.0d0
          overflow_r8 		= huge(tmpr8)**(1023)
	  divbyzero_r8 		= 10./zero8
          underflow_r8 		= tiny(tmpr8)/huge(tmpr8)

!*** IPA optimizes invalid calculations too well, so force ieee_invalid to TRUE
          call set_fpscr_flags(fp_inv_isi)

	  if (overflow_r8 .le.(2**1023))		error stop 249
	  if (divbyzero_r8 .le.(2**1023))		error stop 250
	  if (underflow_r8 .gt.(2**(-1074)))		error stop 251

          flag_values(1) = get_fpscr_flags(fp_overflow)
          flag_values(2) = get_fpscr_flags(fp_div_by_zero)
          flag_values(3) = get_fpscr_flags(fp_invalid)
          flag_values(4) = get_fpscr_flags(fp_underflow)
          flag_values(5) = get_fpscr_flags(fp_inexact)

!*  Check if all flags are set to true.
          if (any(flag_values .eq. 0))           	error stop 252

        end subroutine !!ext_sub14()
!=======================================================================

!*  Subroutine does not have an interface inside main and tries to set flag by calculation.

        subroutine ext_sub15()
	  use xlf_fp_util
          integer(fpscr_kind) :: flag_values(5)
	  integer exp8
	  double precision zero8

	  double precision tmpr8
	  double precision overflow_r8
	  double precision divbyzero_r8
	  double precision underflow_r8

          flag_values(1) = get_fpscr_flags(fp_overflow)
          flag_values(2) = get_fpscr_flags(fp_div_by_zero)
          flag_values(3) = get_fpscr_flags(fp_invalid)
          flag_values(4) = get_fpscr_flags(fp_underflow)
          flag_values(5) = get_fpscr_flags(fp_inexact)

!*  Check if all flags are initially false.
          if (any(flag_values .ne. 0))           	error stop 253

!*  Assign true to all exception flags.
          tmpr8 = 2.0d0
	  zero8	= 0.0d0
          overflow_r8 		= huge(tmpr8)**(1023)
	  divbyzero_r8 		= 10./zero8
          underflow_r8 		= tiny(tmpr8)/huge(tmpr8)

!*** IPA optimizes invalid calculations too well, so force ieee_invalid to TRUE
          call set_fpscr_flags(fp_inv_isi)

	  if (overflow_r8 .le.(2**1023))		error stop 254
	  if (divbyzero_r8 .le.(2**1023))		error stop 255
	  if (underflow_r8 .gt.(2**(-1074)))		error stop 256

          flag_values(1) = get_fpscr_flags(fp_overflow)
          flag_values(2) = get_fpscr_flags(fp_div_by_zero)
          flag_values(3) = get_fpscr_flags(fp_invalid)
          flag_values(4) = get_fpscr_flags(fp_underflow)
          flag_values(5) = get_fpscr_flags(fp_inexact)

!*  Check if all flags are set to true.
          if (any(flag_values .eq. 0))           	error stop 257

        end subroutine !!ext_sub15()
!=======================================================================

!*  Subroutine does not have an interface inside main and sets flag by set_fpscr_flags.

        subroutine ext_sub16()
	  use xlf_fp_util
          integer(fpscr_kind) :: flag_values(5)

          flag_values(1) = get_fpscr_flags(fp_overflow)
          flag_values(2) = get_fpscr_flags(fp_div_by_zero)
          flag_values(3) = get_fpscr_flags(fp_invalid)
          flag_values(4) = get_fpscr_flags(fp_underflow)
          flag_values(5) = get_fpscr_flags(fp_inexact)

!*  Check if all flags are initially false.
          if (any(flag_values .ne. 0))           error stop 258

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
          if (any(flag_values .eq. 0))           error stop 259

        end subroutine !!ext_sub16()
!=======================================================================

!*  Subroutine has an interface inside main and tries to set then clear flag by calculation.

        subroutine ext_sub17()
          use xlf_fp_util
          integer(fpscr_kind) :: flag_values(5)
	  integer exp8
	  double precision zero8

	  double precision tmpr8
	  double precision overflow_r8
	  double precision divbyzero_r8
	  double precision underflow_r8

          flag_values(1) = get_fpscr_flags(fp_overflow)
          flag_values(2) = get_fpscr_flags(fp_div_by_zero)
          flag_values(3) = get_fpscr_flags(fp_invalid)
          flag_values(4) = get_fpscr_flags(fp_underflow)
          flag_values(5) = get_fpscr_flags(fp_inexact)

!*  Check if all flags are initially false.
          if (any(flag_values .ne. 0))           error stop 260

!*  Assign true to all exception flags.
          tmpr8 = 2.0d0
	  zero8	= 0.0d0
          overflow_r8 		= huge(tmpr8)**(1023)
	  divbyzero_r8 		= 10./zero8
          underflow_r8 		= tiny(tmpr8)/huge(tmpr8)

!*** IPA optimizes invalid calculations too well, so force ieee_invalid to TRUE
          call set_fpscr_flags(fp_inv_isi)

	  if (overflow_r8 .le.(2**1023))		error stop 261
	  if (divbyzero_r8 .le.(2**1023))		error stop 262
	  if (underflow_r8 .gt.(2**(-1074)))		error stop 263

          flag_values(1) = get_fpscr_flags(fp_overflow)
          flag_values(2) = get_fpscr_flags(fp_div_by_zero)
          flag_values(3) = get_fpscr_flags(fp_invalid)
          flag_values(4) = get_fpscr_flags(fp_underflow)
          flag_values(5) = get_fpscr_flags(fp_inexact)

!*  Check if all flags are set to true.
          if (any(flag_values .eq. 0))          	 error stop 264

!*  Try to reset flag to false by calculation  -- NOT POSSIBLE
          overflow_r8           = tmpr8
          divbyzero_r8          = 5./1.
          underflow_r8          = tmpr8**(-1)
	  invalid_r8 		= 5./1.

          if (overflow_r8 .ge. (2.0**1023))              error stop 265
          if (divbyzero_r8 .ge. (2.0**1023))             error stop 266
          if (underflow_r8 .lt. (2.0**(-1074)))          error stop 267

          flag_values(1) = get_fpscr_flags(fp_overflow)
          flag_values(2) = get_fpscr_flags(fp_div_by_zero)
          flag_values(3) = get_fpscr_flags(fp_invalid)
          flag_values(4) = get_fpscr_flags(fp_underflow)
          flag_values(5) = get_fpscr_flags(fp_inexact)

!*  Check if all flags are re-set; the should not be.
          if (any(flag_values .eq. 0))                   error stop 268

        end subroutine !!ext_sub17()
!=======================================================================

!*  Subroutine has an interface inside main and sets then clear flag by set_fpscr_flags
!*  and clr_fpscr_flags.
        subroutine ext_sub18()
          use xlf_fp_util
          integer(fpscr_kind) :: flag_values(5)

          flag_values(1) = get_fpscr_flags(fp_overflow)
          flag_values(2) = get_fpscr_flags(fp_div_by_zero)
          flag_values(3) = get_fpscr_flags(fp_invalid)
          flag_values(4) = get_fpscr_flags(fp_underflow)
          flag_values(5) = get_fpscr_flags(fp_inexact)

!*  Check if all flags are initially false.
          if (any(flag_values .ne. 0))           	error stop 269


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
          if (any(flag_values .eq. 0))          	 error stop 270

!*  Try to reset flag to false by calculation
          call clr_fpscr_flags(fp_overflow)
          call clr_fpscr_flags(fp_div_by_zero)
          call clr_fpscr_flags(fp_inv_isi)
          call clr_fpscr_flags(fp_underflow)
          call clr_fpscr_flags(fp_inexact)

          flag_values(1) = get_fpscr_flags(fp_overflow)
          flag_values(2) = get_fpscr_flags(fp_div_by_zero)
          flag_values(3) = get_fpscr_flags(fp_invalid)
          flag_values(4) = get_fpscr_flags(fp_underflow)
          flag_values(5) = get_fpscr_flags(fp_inexact)


!*  Check if all flags are re-set to false.
          if (any(flag_values .ne. 0))          	 error stop 271

        end subroutine !!ext_sub18()
!=======================================================================
!***********************************************************************
!*  External subroutine with exception flags set on entry
!*  into the subroutine. This subroutine doesn't use ieee modules
!*  ----------------------------------------------------------------
!*  Rule:
!*      Calls to procedures that do not use the ieee modules from
!*      procedures that do, will not change the floating point status
!*      except by setting exception flags.
!***********************************************************************
!*  Subroutine has an interface inside main.
        subroutine ext_sub19()
          use xlf_fp_util
          integer(fpscr_kind) :: flag_values(5)

          flag_values(1) = get_fpscr_flags(fp_overflow)
          flag_values(2) = get_fpscr_flags(fp_div_by_zero)
          flag_values(3) = get_fpscr_flags(fp_invalid)
          flag_values(4) = get_fpscr_flags(fp_underflow)
          flag_values(5) = get_fpscr_flags(fp_inexact)

!*  Check if all flags are initially true.
!          if (any(flag_values .eq. 0))                  error stop 272

        end subroutine !!ext_sub19()
!=======================================================================

!*  Subroutine does not have an interface inside main and tries to clear flag by calculation.

        subroutine ext_sub20()
          use xlf_fp_util
          integer(fpscr_kind) :: flag_values(5)
	  integer exp8
	  double precision zero8

	  double precision tmpr8
	  double precision overflow_r8
	  double precision divbyzero_r8
	  double precision underflow_r8

          flag_values(1) = get_fpscr_flags(fp_overflow)
          flag_values(2) = get_fpscr_flags(fp_div_by_zero)
          flag_values(3) = get_fpscr_flags(fp_invalid)
          flag_values(4) = get_fpscr_flags(fp_underflow)
          flag_values(5) = get_fpscr_flags(fp_inexact)

!*  Check if all flags are initially true.
          if (any(flag_values .eq. 0))           error stop 273

	  tmpr8 = 2.0d0
!*  Try to reset flag to false by calculation  -- NOT POSSIBLE
          overflow_r8           = tmpr8
          divbyzero_r8          = 5./1.
          underflow_r8          = tmpr8**(-1)
          invalid_r8            = 5./1.

          if (overflow_r8 .ge. (2.0**1024))              error stop 274
          if (divbyzero_r8 .ge. (2.0**1024))             error stop 275
          if (underflow_r8 .lt. (2.0**(-1074)))          error stop 276

          flag_values(1) = get_fpscr_flags(fp_overflow)
          flag_values(2) = get_fpscr_flags(fp_div_by_zero)
          flag_values(3) = get_fpscr_flags(fp_invalid)
          flag_values(4) = get_fpscr_flags(fp_underflow)
          flag_values(5) = get_fpscr_flags(fp_inexact)

!*  Check if all flags are re-set; they should not be.
          if (any(flag_values .eq. 0))                   error stop 277

        end subroutine !!ext_sub20()
!=======================================================================

!*  Subroutine does not have an interface inside main and sets flag by set_fpscr_flags
!*  and then clears flag by clr_fpscr_flags.
        subroutine ext_sub21()
          use xlf_fp_util
          integer(fpscr_kind) :: flag_values(5)

          flag_values(1) = get_fpscr_flags(fp_overflow)
          flag_values(2) = get_fpscr_flags(fp_div_by_zero)
          flag_values(3) = get_fpscr_flags(fp_invalid)
          flag_values(4) = get_fpscr_flags(fp_underflow)
          flag_values(5) = get_fpscr_flags(fp_inexact)

!*  Check if all flags are initially true.
          if (any(flag_values .eq. 0))                  error stop 278

!*  Try to reset flag to false by using clr_fpscr_flags()
          call clr_fpscr_flags(fp_overflow)
          call clr_fpscr_flags(fp_div_by_zero)
          call clr_fpscr_flags(fp_underflow)
          call clr_fpscr_flags(fp_inexact)

!*** Clear all invalid possibilies
          call clr_fpscr_flags(fp_inv_isi)
	  call clr_fpscr_flags(fp_inv_idi)
	  call clr_fpscr_flags(fp_inv_zdz)
	  call clr_fpscr_flags(fp_inv_imz)
	  call clr_fpscr_flags(fp_inv_cmp)
	  call clr_fpscr_flags(fp_inv_sqrt)
	  call clr_fpscr_flags(fp_inv_cvi)
	  call clr_fpscr_flags(fp_inv_vxsoft)

          flag_values(1) = get_fpscr_flags(fp_overflow)
          flag_values(2) = get_fpscr_flags(fp_div_by_zero)
          flag_values(3) = get_fpscr_flags(fp_invalid)
          flag_values(4) = get_fpscr_flags(fp_underflow)
          flag_values(5) = get_fpscr_flags(fp_inexact)

!*  Check if all flags are re-set to false.
          if (any(flag_values .ne. 0))                   error stop 279

        end subroutine !!ext_sub21()
!=======================================================================

