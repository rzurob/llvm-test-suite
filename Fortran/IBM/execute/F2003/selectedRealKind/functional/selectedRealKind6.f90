!***********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/24/2007
!*
!*  PRIMARY FUNCTIONS TESTED   : -4 return value from SELECTED_REAL_KIND ([P, R])
!*                               intrinsic
!*  SECONDARY FUNCTIONS TESTED : see below
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : The testcase is testing the -4 return
!*                               case from selected_real_kind, when return
!*                               values from selected_real_kind are passed
!*				 as arguments to other functions.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

	program selectedRealKind6
		integer result
		interface
		integer function foo(N)
		 integer N
		end function
		end interface
		result = foo(selected_real_kind(16,300))
		if (result .ne. -4 )	error stop 10_4

	end program

	integer function foo(N)
		integer N
		foo = N
	end function

