!*********************************************************************
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : April 15, 2002
!*
!*  PRIMARY FUNCTIONS TESTED   : ieee_get_status()
!*				 ieee_set_status()
!*
!*  REFERENCE                  : Feature 180920
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : This testcase checks
!*				 ieee_get_status() and ieee_set_status()
!*				 functions inside subroutines which are
!*				 in modules that use IEEE.
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

      module sub1mod
        use ieee_arithmetic

	contains
        subroutine ext_sub1()
          implicit none

          type(ieee_status_type) :: status_value
          logical*4 flag_values(5)
          logical*4 hlt_values(5)
          type(ieee_round_type) :: round_value

!***********************************************************************
!*  Test 1: Restore all flags to false and rounding mode to ieee_nearest.
!*
!*	Call ieee_get_status, to get the initial status of all flags and
!*	rounding mode - all flags should be false and rounding mode
!*	should be ieee_nearest
!*      Then set all flags to true and rounding mode to ieee_up, then
!*      call ieee_set_status to make sure that all flags are restored to
!*      false, and rounding mode restored to ieee_nearest.
!*
!***********************************************************************

!*  Check if initially all flags are false and rounding mode is ieee_nearest
          call ieee_get_flag(ieee_all, flag_values)
  	  if (any(flag_values .neqv. .false.))		error stop 1

          call ieee_get_halting_mode(ieee_all, hlt_values)
  	  if (any(hlt_values .neqv. .false.))		error stop 101

          call ieee_get_rounding_mode(round_value)
          if (round_value /= ieee_nearest)              error stop 201


!*  Get the current value of the floating point status
	  call ieee_get_status(status_value)


!*  Set all exception flags to true, and rounding mode to ieee_up.
	  call ieee_set_flag(ieee_all, .true.)
          call ieee_get_flag(ieee_all, flag_values)
  	  if (any(flag_values .neqv. .true.))		error stop 2

          call ieee_set_halting_mode(ieee_all, .true.)
          call ieee_get_halting_mode(ieee_all, hlt_values)
          if (any(hlt_values .neqv. .true.))          	error stop 102

          call ieee_set_rounding_mode(ieee_up)
          call ieee_get_rounding_mode(round_value)
          if (round_value /= ieee_up)                   error stop 202


!*  Restore the floating point status.
	  call ieee_set_status(status_value)

!*  Check if all flags are restored to false, and rounding mode is
!*  restored to ieee_nearest.
          call ieee_get_flag(ieee_all, flag_values)
  	  if (any(flag_values .neqv. .false.))		error stop 3

          call ieee_get_halting_mode(ieee_all, hlt_values)
          if (any(hlt_values .neqv. .false.))          	error stop 103

          call ieee_get_rounding_mode(round_value)
          if (round_value /= ieee_nearest)              error stop 203
        end subroutine
      end module
!=======================================================================

      module sub2mod
        use ieee_arithmetic
        contains
        subroutine ext_sub2()
          implicit none

          type(ieee_status_type) :: status_value
          logical*4 flag_values(5)
          logical*4 hlt_values(5)
          type(ieee_round_type) :: round_value

!***********************************************************************
!*  Test 2: Restore all flags to true and rounding mode to ieee_down.
!*
!*      Set all flags to true and rounding mode to ieee_down, then call
!*      ieee_get_status to get the floating point status.
!*	Set all flags to false and rounding mode to ieee_to_zero,
!*      then call ieee_set_status to make sure that all flags are
!*      are restored to true and rounding mode is restored to ieee_down.
!*
!***********************************************************************

!*  Set all flags to true and rounding mode to ieee_down.
          call ieee_set_flag(ieee_all, .true.)
          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .true.))           error stop 4

          call ieee_set_halting_mode(ieee_all, .true.)
          call ieee_get_halting_mode(ieee_all, hlt_values)
          if (any(hlt_values .neqv. .true.))            error stop 104

          call ieee_set_rounding_mode(ieee_down)
          call ieee_get_rounding_mode(round_value)
          if (round_value /= ieee_down)                	error stop 204

!*  Get the current value of the floating point status
          call ieee_get_status(status_value)


!*  Set all flags to false, and rounding mode to ieee_to_zero.
          call ieee_set_flag(ieee_all, .false.)
          call ieee_get_flag(ieee_all, flag_values)
  	  if (any(flag_values .neqv. .false.))		error stop 5

          call ieee_set_halting_mode(ieee_all, .false.)
          call ieee_get_halting_mode(ieee_all, hlt_values)
          if (any(hlt_values .neqv. .false.))       	error stop 105

          call ieee_set_rounding_mode(ieee_to_zero)
          call ieee_get_rounding_mode(round_value)
          if (round_value /= ieee_to_zero)           	error stop 205


!*  Restore the floating point status.
          call ieee_set_status(status_value)

!*  Check if all flags are restored to true, and rounding mode is
!*  restored to ieee__down.
          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .true.))          	error stop 6

          call ieee_get_halting_mode(ieee_all, hlt_values)
          if (any(hlt_values .neqv. .true.))            error stop 106

          call ieee_get_rounding_mode(round_value)
          if (round_value /= ieee_down)                	error stop 206
        end subroutine
      end module
!=======================================================================

      module sub3mod
        use ieee_arithmetic
        contains
        subroutine ext_sub3()
          implicit none

          type(ieee_status_type) :: status_value
          logical*4 flag_values(5)
          logical*4 hlt_values(5)
          type(ieee_round_type) :: round_value

!***********************************************************************
!*  Test 3:
!*      For exception flags:
!*      Set overflow, invalid, inexact flags to false and
!*      divide_by_zero and underflow flags to true.
!*      Call ieee_get_status to get the status of all exception flags.
!*      Then change overflow, invalid, inexact flags to true and set
!*      divide_by_zero and underflow flags as false.
!*      Call ieee_set_status to make sure that all flags are restored to
!*      their values before calling ieee_get_status.
!*
!*      For halting flags:
!*      Set overflow, invalid, inexact flags to true; and set
!*      divide_by_zero and underflow flags as false.
!*      Call ieee_get_status to get the status of all flags.
!*      Set overflow, invalid, inexact flags to false and
!*      divide_by_zero and underflow flags as true.
!*      Call ieee_set_status to make sure that all flags are restored to
!*      their values before calling ieee_get_status.
!***********************************************************************

!*  Exception flags:  Keep overflow, invalid, inexact flags as
!*  false and divide_by_zero and underflow flags as true.
          call ieee_set_flag(ieee_divide_by_zero, .true.)
          call ieee_set_flag(ieee_underflow, .true.)

          call ieee_get_flag(ieee_all, flag_values)
          if (flag_values(1) .neqv. .false.)            error stop 7
          if (flag_values(2) .neqv. .true.)             error stop 8
          if (flag_values(3) .neqv. .false.)            error stop 9
          if (flag_values(4) .neqv. .true.)             error stop 10
          if (flag_values(5) .neqv. .false.)            error stop 11

!*  Halting flags:  Set overflow, invalid, inexact flags to
!*  true and keep divide_by_zero and underflow flags as false.
          call ieee_set_halting_mode(ieee_overflow, .true.)
          call ieee_set_halting_mode(ieee_invalid, .true.)
          call ieee_set_halting_mode(ieee_inexact, .true.)

          call ieee_get_halting_mode(ieee_all, hlt_values)
          if (hlt_values(1) .neqv. .true.)        	error stop 107
          if (hlt_values(2) .neqv. .false.)        	error stop 108
          if (hlt_values(3) .neqv. .true.)        	error stop 109
          if (hlt_values(4) .neqv. .false.)        	error stop 110
          if (hlt_values(5) .neqv. .true.)        	error stop 111

          call ieee_get_status(status_value)

!*  Exception flags:  Change overflow, invalid, inexact flags to true;
!*  and set divide_by_zero and underflow flags as false.
	  call ieee_set_flag(ieee_overflow, .true.)
	  call ieee_set_flag(ieee_divide_by_zero, .false.)
          call ieee_set_flag(ieee_invalid, .true.)
	  call ieee_set_flag(ieee_underflow, .false.)
          call ieee_set_flag(ieee_inexact, .true.)
          call ieee_get_flag(ieee_all, flag_values)

          if (flag_values(1) .neqv. .true.)           	error stop 12
          if (flag_values(2) .neqv. .false.)           	error stop 13
          if (flag_values(3) .neqv. .true.)           	error stop 14
          if (flag_values(4) .neqv. .false.)           	error stop 15
          if (flag_values(5) .neqv. .true.)           	error stop 16

!*  Halting flags:  Change overflow, invalid, inexact flags to false;
!*  and set divide_by_zero and underflow flags as true.
          call ieee_set_halting_mode(ieee_overflow, .false.)
          call ieee_set_halting_mode(ieee_divide_by_zero, .true.)
          call ieee_set_halting_mode(ieee_invalid, .false.)
          call ieee_set_halting_mode(ieee_underflow, .true.)
          call ieee_set_halting_mode(ieee_inexact, .false.)

          call ieee_get_halting_mode(ieee_all, hlt_values)
          if (hlt_values(1) .neqv. .false.)        	error stop 112
          if (hlt_values(2) .neqv. .true.)         	error stop 113
          if (hlt_values(3) .neqv. .false.)        	error stop 114
          if (hlt_values(4) .neqv. .true.)         	error stop 115
          if (hlt_values(5) .neqv. .false.)        	error stop 116


!*  Restore all flags to their status before calling get_status.
          call ieee_set_status(status_value)

!*  Exception flags
          call ieee_get_flag(ieee_all, flag_values)
          if (flag_values(1) .neqv. .false.)            error stop 17
          if (flag_values(2) .neqv. .true.)             error stop 18
          if (flag_values(3) .neqv. .false.)            error stop 19
          if (flag_values(4) .neqv. .true.)             error stop 20
          if (flag_values(5) .neqv. .false.)            error stop 21

!*  Halting flags
          call ieee_get_halting_mode(ieee_all, hlt_values)
          if (hlt_values(1) .neqv. .true.)         	error stop 117
          if (hlt_values(2) .neqv. .false.)        	error stop 118
          if (hlt_values(3) .neqv. .true.)         	error stop 119
          if (hlt_values(4) .neqv. .false.)        	error stop 120
          if (hlt_values(5) .neqv. .true.)         	error stop 121
        end subroutine
      end module
!=======================================================================

      module sub4mod
        use ieee_arithmetic
        contains
        subroutine ext_sub4()
          implicit none

          type(ieee_status_type) :: status_value
          logical*4 flag_values(5)
          logical*4 hlt_values(5)
          type(ieee_round_type) :: round_value

!***********************************************************************
!*  Test 4: Opposite of Test 3.
!*
!*      For exception flags:
!*      Set overflow, invalid, inexact flags to true; and set
!*      divide_by_zero and underflow flags as false.
!*      Call ieee_get_status to get the status of all flags.
!*      Set overflow, invalid, inexact flags to false and
!*      divide_by_zero and underflow flags as true.
!*      Call ieee_set_status to make sure that all flags are restored to
!*      their values before calling ieee_get_status.
!*
!*      For halting flags:
!*      Set overflow, invalid, inexact flags to false and
!*      divide_by_zero and underflow flags to true.
!*      Call ieee_get_status to get the status of all flags.
!*      Then change overflow, invalid, inexact flags to true; and set
!*      divide_by_zero and underflow flags as false.
!*      Call ieee_set_status to make sure that all flags are restored to
!*      their values before calling ieee_get_status.
!*
!***********************************************************************

!*  Exception flags:  Set overflow, invalid, inexact flags to
!*  true, and keep divide_by_zero and underflow flags as false.

          call ieee_set_flag(ieee_overflow, .true.)
          call ieee_set_flag(ieee_invalid, .true.)
          call ieee_set_flag(ieee_inexact, .true.)

          call ieee_get_flag(ieee_all, flag_values)
          if (flag_values(1) .neqv. .true.)             error stop 22
          if (flag_values(2) .neqv. .false.)            error stop 23
          if (flag_values(3) .neqv. .true.)             error stop 24
          if (flag_values(4) .neqv. .false.)            error stop 25
          if (flag_values(5) .neqv. .true.)             error stop 26

!*  Halting flags:  Keep overflow, invalid, inexact flags as
!*  false, and set divide_by_zero and underflow flags to true.
          call ieee_set_halting_mode(ieee_divide_by_zero, .true.)
          call ieee_set_halting_mode(ieee_underflow, .true.)

          call ieee_get_halting_mode(ieee_all, hlt_values)
          if (hlt_values(1) .neqv. .false.)        	error stop 122
          if (hlt_values(2) .neqv. .true.)         	error stop 123
          if (hlt_values(3) .neqv. .false.)        	error stop 124
          if (hlt_values(4) .neqv. .true.)         	error stop 125
          if (hlt_values(5) .neqv. .false.)        	error stop 126

          call ieee_get_status(status_value)

!*  Exception flags:  Change overflow, invalid, inexact flags to
!*  false, and divide_by_zero and underflow flags as true.
          call ieee_set_flag(ieee_overflow, .false.)
          call ieee_set_flag(ieee_divide_by_zero, .true.)
          call ieee_set_flag(ieee_invalid, .false.)
          call ieee_set_flag(ieee_underflow, .true.)
          call ieee_set_flag(ieee_inexact, .false.)

          call ieee_get_flag(ieee_all, flag_values)
          if (flag_values(1) .neqv. .false.)            error stop 27
          if (flag_values(2) .neqv. .true.)             error stop 28
          if (flag_values(3) .neqv. .false.)            error stop 29
          if (flag_values(4) .neqv. .true.)             error stop 30
          if (flag_values(5) .neqv. .false.)            error stop 31

!*  Halting flags:  Set overflow, invalid, inexact flags to
!*  true, and divide_by_zero and underflow flags as false.
          call ieee_set_halting_mode(ieee_overflow, .true.)
          call ieee_set_halting_mode(ieee_divide_by_zero, .false.)
          call ieee_set_halting_mode(ieee_invalid, .true.)
          call ieee_set_halting_mode(ieee_underflow, .false.)
          call ieee_set_halting_mode(ieee_inexact, .true.)

          call ieee_get_halting_mode(ieee_all, hlt_values)
          if (hlt_values(1) .neqv. .true.)         	error stop 127
          if (hlt_values(2) .neqv. .false.)        	error stop 128
          if (hlt_values(3) .neqv. .true.)         	error stop 129
          if (hlt_values(4) .neqv. .false.)        	error stop 130
          if (hlt_values(5) .neqv. .true.)         	error stop 131


!*  Restore all flags to their status before calling get_status.
          call ieee_set_status(status_value)

!*  Exception flags
          call ieee_get_flag(ieee_all, flag_values)
          if (flag_values(1) .neqv. .true.)             error stop 32
          if (flag_values(2) .neqv. .false.)            error stop 33
          if (flag_values(3) .neqv. .true.)             error stop 34
          if (flag_values(4) .neqv. .false.)            error stop 35
          if (flag_values(5) .neqv. .true.)             error stop 36

!*  Halting flags
          call ieee_get_halting_mode(ieee_all, hlt_values)
          if (hlt_values(1) .neqv. .false.)        	error stop 132
          if (hlt_values(2) .neqv. .true.)         	error stop 133
          if (hlt_values(3) .neqv. .false.)        	error stop 134
          if (hlt_values(4) .neqv. .true.)         	error stop 135
          if (hlt_values(5) .neqv. .false.)        	error stop 136
        end subroutine
      end module
!=======================================================================

        program ieeestatus04
      	  use sub1mod
      	  use sub2mod
      	  use sub3mod
      	  use sub4mod

          call ext_sub1()
          call ext_sub2()
          call ext_sub3()
          call ext_sub4()

        end program

