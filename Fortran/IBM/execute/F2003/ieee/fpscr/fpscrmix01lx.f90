!*********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 -qstrict
! %GROUP: fpscrmix01lx.f
! %VERIFY:
! %STDIN:
! %STDOUT: fpscrmix01lx.out
! %EXECARGS:
! %POSTCMD: rm -f fpscrmix01lx.out *.mod
! %END
!**********************************************************************
!*  ===================================================================
!*  AIX XL FORTRAN/6000 TEST CASE                 IBM INTERNAL USE ONLY
!*  ===================================================================
!*  TEST CASE TITLE            : IEEE modules - FPSCR save and restore
!*
!*  PROGRAMMER                 : Kobi Vinayagamoorthy
!*  DATE                       : April 15, 2002
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : ieee_get_flag()
!*				 ieee_set_flag()
!*				 ieee_get_halting_mode()
!*				 ieee_set_halting_mode()
!*				 ieee_get_rounding_mode()
!*				 ieee_set_rounding_mode()
!*
!*  REFERENCE                  : Feature 180920
!*
!*  DRIVER STANZA              : xlf95
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : This testcase checks to see if FPSCR
!*				 save and restore is properly done in 
!*				 subroutines inside modules that use IEEE.
!*
!*				 The main program calls subroutine from 
!*				 module, and module also calls subroutine 
!*				 from other module.
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

!***********************************************************************
!* module sub3mod
!*
!*      This module is used by module sub1mod.
!***********************************************************************

      module sub3mod
        use ieee_arithmetic
        implicit none

        logical*4 flag_values(5)
        logical*4 hlt_values(5)
        type(ieee_round_type) :: round_value

        contains

        subroutine ext_sub3()

!* ---------------------------------------------------------------------
!*      For exception flags:
!*      Set overflow, invalid, inexact flags to false and
!*      divide_by_zero and underflow flags to true.
!*
!*      For halting flags:
!*      Set overflow, invalid, inexact flags to true; and set
!*      divide_by_zero and underflow flags as false.
!*
!*      For rounding mode:
!*      Set rounding mode to ieee_to_zero.
!* ---------------------------------------------------------------------

!*  Check to see on entry, exception flag is cleared to false;
!*  halting flag is not modified and is true;
!*  and rounding mode is also not modified and is ieee_up.

          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .false.))          error stop 19

          call ieee_get_halting_mode(ieee_all, hlt_values)
          if (any(hlt_values .neqv. .false.))           error stop 20

          call ieee_get_rounding_mode(round_value)
          if (round_value /= ieee_up)                   error stop 21

!*  Exception flags:  Keep overflow, invalid, inexact flags as
!*  false and divide_by_zero and underflow flags as true.

          call ieee_set_flag(ieee_divide_by_zero, .true.)
          call ieee_set_flag(ieee_underflow, .true.)

          call ieee_get_flag(ieee_all, flag_values)
          if (flag_values(1) .neqv. .false.)            error stop 22
          if (flag_values(2) .neqv. .true.)             error stop 23
          if (flag_values(3) .neqv. .false.)            error stop 24
          if (flag_values(4) .neqv. .true.)             error stop 25
          if (flag_values(5) .neqv. .false.)            error stop 26


!*  Halting flags:  Set overflow, invalid, inexact flags as
!*  true and keep divide_by_zero and underflow flags to false.
          call ieee_set_halting_mode(ieee_overflow, .true.)
          call ieee_set_halting_mode(ieee_invalid, .true.)
          call ieee_set_halting_mode(ieee_inexact, .true.)

          call ieee_get_halting_mode(ieee_all, hlt_values)
          if (hlt_values(1) .neqv. .true.)              error stop 27
          if (hlt_values(2) .neqv. .false.)             error stop 28
          if (hlt_values(3) .neqv. .true.)              error stop 29
          if (hlt_values(4) .neqv. .false.)             error stop 30
          if (hlt_values(5) .neqv. .true.)              error stop 31

!*  Rounding mode:  Change mode to ieee_to_zero.
          call ieee_set_rounding_mode(ieee_to_zero)
          call ieee_get_rounding_mode(round_value)
          if (round_value /= ieee_to_zero)              error stop 32

        end subroutine
      end module
!=======================================================================

!***********************************************************************
!* module sub1mod:
!*
!*	In this module, initial floating point status is verified,
!*	then modified and the modification is verified.
!*
!*	The modified values are passed to another soubroutine 
!* 	from another module that uses IEEE, then the floating point
!*	status returned from that subroutine is verified.
!*
!***********************************************************************

      module sub1mod
!*  Indirect use of IEEE by module sub1mod:
!*  module sub1mod uses IEEE because module sub3mod uses IEEE.
        use sub3mod
        implicit none

	contains

        subroutine ext_sub1()

!* ---------------------------------------------------------------------
!*	Get the initial value of all flags and rounding mode -
!*	all flags should be false and rounding mode should be ieee_nearest
!*      Then set all flags to true and rounding mode to ieee_up. 
!* ---------------------------------------------------------------------

!*  Check if initially all flags are false and rounding mode is ieee_nearest
          call ieee_get_flag(ieee_all, flag_values)
  	  if (any(flag_values .neqv. .false.))		error stop 1

          call ieee_get_halting_mode(ieee_all, hlt_values)
  	  if (any(hlt_values .neqv. .false.))		error stop 2

          call ieee_get_rounding_mode(round_value)
          if (round_value /= ieee_nearest)              error stop 3

!*  Set all exception flags to true, and rounding mode to ieee_up.
	  call ieee_set_flag(ieee_all, .true.)
          call ieee_get_flag(ieee_all, flag_values)
  	  if (any(flag_values .neqv. .true.))		error stop 4

!          call ieee_set_halting_mode(ieee_all, .true.)
!          call ieee_get_halting_mode(ieee_all, hlt_values)
!          if (any(hlt_values .neqv. .true.))          	error stop 5

          call ieee_set_rounding_mode(ieee_up)
          call ieee_get_rounding_mode(round_value)
          if (round_value /= ieee_up)                   error stop 6

!* ---------------------------------------------------------------------
!*      Call a subroutine from another module that uses IEEE.
!*      Check the value of all flags and rounding mode.
!*      All exception flags should be true, halting mode should be false  
!*      and rounding mode should be ieee_up.
!*      This is because exception flags cannot be cleared and 
!*	halting mode and rounding mode cannot be modified on exit from
!*	procedure.
!* ---------------------------------------------------------------------

!*  Call a subroutine from another module that uses IEEE.
	  call ext_sub3()

!*  Check to see that flags are not cleared, and that halting mode
!*  flags and rounding mode are the same as when entering into the 
!*  subroutine.
          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .true.))           error stop 7

          call ieee_get_halting_mode(ieee_all, hlt_values)
          if (any(hlt_values .neqv. .false.))           error stop 8

          call ieee_get_rounding_mode(round_value)
          if (round_value /= ieee_up)                   error stop 9

        end subroutine
      end module
!=======================================================================

!***********************************************************************
!* module sub2mod:
!*
!*      In this module, floating point status is modified in the first
!*	subroutine and then passed to the second subroutine in this
!*	module. Floating point status is verified on entry into the 
!*	second subroutine.
!*
!***********************************************************************

      module sub2mod
        use ieee_arithmetic
        logical*4 flag_values2(5)
        logical*4 hlt_values2(5)
        type(ieee_round_type) :: round_value

        contains

        subroutine ext_sub2a()

!*  Check if initially all flags are false and rounding mode is ieee_nearest
          call ieee_get_flag(ieee_all, flag_values2)
          if (any(flag_values2 .neqv. .false.))          error stop 10

          call ieee_get_halting_mode(ieee_all, hlt_values2)
          if (any(hlt_values2 .neqv. .false.))           error stop 11

          call ieee_get_rounding_mode(round_value)
          if (round_value /= ieee_nearest)              error stop 12

!*  Set all exception flags to true, halting mode are kept false
!*  and rounding mode is set to ieee_down. 
          call ieee_set_flag(ieee_all, .true.)
          call ieee_get_flag(ieee_all, flag_values2)
          if (any(flag_values2 .neqv. .true.))           error stop 13

          call ieee_get_halting_mode(ieee_all, hlt_values2)
          if (any(hlt_values2 .neqv. .false.))           error stop 14

          call ieee_set_rounding_mode(ieee_down)
          call ieee_get_rounding_mode(round_value)
          if (round_value /= ieee_down)                	error stop 15

        end subroutine
!* ---------------------------------------------------------------------

        subroutine ext_sub2b()

!*  Check to see on entry, exception flags are cleared to false;
!*  halting flags are not modified and are all false;
!*  and rounding mode is also not modified and is ieee_down.

          call ieee_get_flag(ieee_all, flag_values2)
          if (any(flag_values2 .neqv. .false.))          error stop 16

          call ieee_get_halting_mode(ieee_all, hlt_values2)
          if (any(hlt_values2 .neqv. .false.))          	error stop 17

          call ieee_get_rounding_mode(round_value)
          if (round_value /= ieee_nearest)             	error stop 18

        end subroutine

      end module
!=======================================================================

!***********************************************************************
!* module sub4mod:
!*
!*	The exception flags are modified in the subroutine that is in 
!*	this module and then modified again.
!*	On return from this subroutine, all exception flags will be set 
!*	true, since they are not allowed to be cleared on exit from a
!*	procedure.
!*
!*	Halting flags and rounding mode will also be modified, but they will 
!* 	not change on return from this subroutine (they will be the same 
!*	as when entering the subroutine).
!*
!***********************************************************************

      module sub4mod
        use ieee_arithmetic
        implicit none

        logical*4 flag_values4(5)
        logical*4 hlt_values4(5)
        type(ieee_round_type) :: round_value

        contains
        subroutine ext_sub4()

!* ---------------------------------------------------------------------
!*      For exception flags:
!*      Set overflow, invalid, inexact flags to true; and set
!*      divide_by_zero and underflow flags as false.
!*      Then set overflow, invalid, inexact flags to false and 
!*      divide_by_zero and underflow flags as true.
!*
!*      For halting flags:
!*      Set overflow, invalid, inexact flags to false and 
!*      divide_by_zero and underflow flags to true.
!*      Then change overflow, invalid, inexact flags to true; and set
!*      divide_by_zero and underflow flags as false.
!*
!*      For rounding mode:
!*      Set rounding mode to ieee_to_zero.  Thne change rounding mode
!*	to ieee_up.
!* ---------------------------------------------------------------------

!*  Exception flags:  Set overflow, invalid, inexact flags to
!*  true, and divide_by_zero and underflow flags to false.

          call ieee_set_flag(ieee_overflow, .true.)
          call ieee_set_flag(ieee_divide_by_zero, .false.)
          call ieee_set_flag(ieee_invalid, .true.)
          call ieee_set_flag(ieee_underflow, .false.)
          call ieee_set_flag(ieee_inexact, .true.)

          call ieee_get_flag(ieee_all, flag_values4)
          if (flag_values4(1) .neqv. .true.)             error stop 33
          if (flag_values4(2) .neqv. .false.)            error stop 34
          if (flag_values4(3) .neqv. .true.)             error stop 35
          if (flag_values4(4) .neqv. .false.)            error stop 36
          if (flag_values4(5) .neqv. .true.)             error stop 37

!*  Halting flags:  Set overflow, invalid, inexact flags as
!*  false, and set divide_by_zero and underflow flags to true.
          call ieee_set_halting_mode(ieee_overflow, .false.)
          call ieee_set_halting_mode(ieee_divide_by_zero, .true.)
          call ieee_set_halting_mode(ieee_invalid, .false.)
          call ieee_set_halting_mode(ieee_underflow, .true.)
          call ieee_set_halting_mode(ieee_inexact, .false.)

          call ieee_get_halting_mode(ieee_all, hlt_values4)
          if (hlt_values4(1) .neqv. .false.)        	error stop 38
          if (hlt_values4(2) .neqv. .true.)         	error stop 39
          if (hlt_values4(3) .neqv. .false.)        	error stop 40
          if (hlt_values4(4) .neqv. .true.)         	error stop 41
          if (hlt_values4(5) .neqv. .false.)        	error stop 42

!*  Rounding mode:  Set rounding mode to ieee_nearest.
          call ieee_set_rounding_mode(ieee_nearest)
          call ieee_get_rounding_mode(round_value)
          if (round_value /= ieee_nearest)              error stop 43

          call ieee_set_halting_mode(ieee_all, .false.)
          call ieee_get_halting_mode(ieee_all, hlt_values4)
          if (any(hlt_values4 .neqv. .false.))           error stop 80

!*  Exception flags:  Change overflow, invalid, inexact flags to 
!*  false, and divide_by_zero and underflow flags as true.
          call ieee_set_flag(ieee_overflow, .false.)
          call ieee_set_flag(ieee_divide_by_zero, .true.)
          call ieee_set_flag(ieee_invalid, .false.)
          call ieee_set_flag(ieee_underflow, .true.)
          call ieee_set_flag(ieee_inexact, .false.)

          call ieee_get_flag(ieee_all, flag_values4)
          if (flag_values4(1) .neqv. .false.)            error stop 44
          if (flag_values4(2) .neqv. .true.)             error stop 45
          if (flag_values4(3) .neqv. .false.)            error stop 46
          if (flag_values4(4) .neqv. .true.)             error stop 47
          if (flag_values4(5) .neqv. .false.)            error stop 48

!*  Halting flags:  Set overflow, invalid, inexact flags to
!*  true, and divide_by_zero and underflow flags as false.
          call ieee_set_halting_mode(ieee_overflow, .true.)
          call ieee_set_halting_mode(ieee_divide_by_zero, .false.)
          call ieee_set_halting_mode(ieee_invalid, .true.)
          call ieee_set_halting_mode(ieee_underflow, .false.)
          call ieee_set_halting_mode(ieee_inexact, .true.)

          call ieee_get_halting_mode(ieee_all, hlt_values4)
          if (hlt_values4(1) .neqv. .true.)         	error stop 49
          if (hlt_values4(2) .neqv. .false.)        	error stop 50
          if (hlt_values4(3) .neqv. .true.)         	error stop 51
          if (hlt_values4(4) .neqv. .false.)        	error stop 52
          if (hlt_values4(5) .neqv. .true.)         	error stop 53

!*  Rounding mode:  Change rounding mode to ieee_up.
          call ieee_set_rounding_mode(ieee_up)
          call ieee_get_rounding_mode(round_value)
          if (round_value /= ieee_up)              	error stop 54

        end subroutine
      end module
!=======================================================================

        program fpscrmix01lx
      	  use sub1mod
      	  use sub2mod
      	  use sub4mod

!*  Check if initially all flags are false and rounding mode is ieee_nearest
          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .false.))      	error stop 60

          call ieee_get_halting_mode(ieee_all, hlt_values)
          if (any(hlt_values .neqv. .false.))           error stop 61

          call ieee_get_rounding_mode(round_value)
          if (round_value /= ieee_nearest)              error stop 62


          call ext_sub1()
!*  Check on return that exception flags are set, and halting mode flags
!*  and rounding mode are the same as when entering the subroutine. 
          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .true.))        	error stop 63

          call ieee_get_halting_mode(ieee_all, hlt_values)
          if (any(hlt_values .neqv. .false.))          	error stop 64

          call ieee_get_rounding_mode(round_value)
          if (round_value /= ieee_nearest)             	error stop 65


          call ext_sub2a()
!*  Check on return, the exception flags are set, and halting mode flags
!*  and rounding mode are the same as when entering the subroutine. 
          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .true.))          	error stop 66

          call ieee_get_halting_mode(ieee_all, hlt_values)
          if (any(hlt_values .neqv. .false.))           error stop 67

          call ieee_get_rounding_mode(round_value)
          if (round_value /= ieee_nearest)            	error stop 68


          call ext_sub2b()
!*  Check on return, the exception flags are set, and halting mode flags
!*  and rounding mode are the same as when entering the subroutine. 
          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .true.))         	error stop 69

          call ieee_get_halting_mode(ieee_all, hlt_values)
          if (any(hlt_values .neqv. .false.))           error stop 70

          call ieee_get_rounding_mode(round_value)
          if (round_value /= ieee_nearest)              error stop 71


          call ext_sub4()
!*  Check on return, the exception flags are set, and halting mode flags
!*  and rounding mode are the same as when entering the subroutine. 
          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .true.))          	error stop 72

          call ieee_get_halting_mode(ieee_all, hlt_values)
          if (any(hlt_values .neqv. .false.))           error stop 73

          call ieee_get_rounding_mode(round_value)
          if (round_value /= ieee_nearest)              error stop 74

        end program


