!***********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : selectedRealKind5
!*  TEST CASE TITLE            :
!*
!*
!*  PROGRAMMER                 : Salma Elshatanoufy
!*  DATE                       : 08/24/2007
!*  ORIGIN                     : XL Fortran Compiler Development, IBM Torolab
!*
!*  PRIMARY FUNCTIONS TESTED   : -4 return value from SELECTED_REAL_KIND([P, R])
!*                               intrinsic
!*  SECONDARY FUNCTIONS TESTED : see below
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  :
!*
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : The testcase is testing the -4 return
!*                               case from selected_real_kind, when function
!*                               return values and variables are used.
!*
!*
!*
!234567890123456789012345678901234567890123456789012345678901234567890

	program selectedRealKind5
		integer k, j, l, m, n
		interface
		integer function foo(N)
			integer N
		end function
		end interface
		k = 292
		j = 16
		n = -4
		l = selected_real_kind ( j, k)
		if(l .ne. n) error stop 10_4

		m = selected_real_kind (foo(j), foo(k))
		if (m .ne. n) error stop 20_4

	end program
	
	integer function foo(N)
		integer N
		foo = N
	end function

