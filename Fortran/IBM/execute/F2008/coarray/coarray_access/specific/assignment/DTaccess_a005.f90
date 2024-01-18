!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : DTaccess_a005.f
!*
!*  PROGRAMMER                 : Francesco Cassullo
!*  DATE                       : March 2011
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  DESCRIPTION
!*
!*  Assign simple values to a Derived Type coarray's logical components
!*  (scalars and arrays of different kinds)
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program main

	type obj
		logical(1) :: l1
		logical(2) :: l2
		logical(4) :: l4
		logical(8) :: l8
	end type

	logical, parameter :: F = .false., T = .true.
	logical :: T1

	type (obj), save :: caf[*], cafar(10)[*]


	T1 = (command_argument_count() < 10)
	! set scalars to TRUE and arrays to a mix of T/F
	caf%l1 = T1
	caf%l2 = T1
	caf%l4 = T1
	caf%l8 = T1

	cafar(:)%l1 = [T1,T1,T1,F,T1,F,F,T1,F,F] ! sort-of Fibonacci
	cafar(1:9:2)%l2 = T1 				! odd elements
	cafar([2,4,6,8,10])%l4 = T1 			! even elements
	cafar(9:1:-2)%l8 = T1 				! odd elements again

	if ( (.not. caf%l1) .or. (.not. caf%l2) .or. (.not. caf%l4) .or. (.not. caf%l8) ) error stop 2
	if (any(cafar(:)%l1 .neqv. [T,T,T,F,T,F,F,T,F,F])) error stop 3
	if (any(cafar(:)%l2 .neqv. [T,F,T,F,T,F,T,F,T,F])) error stop 4
	if (any(cafar(:)%l4 .neqv. [F,T,F,T,F,T,F,T,F,T])) error stop 5
	if (any(cafar(:)%l8 .neqv. [T,F,T,F,T,F,T,F,T,F])) error stop 6

	! set scalars to FALSE and arrays to a different mix of T/F
	caf%l1 = F
	caf%l2 = F
	caf%l4 = F
	caf%l8 = F

	cafar(:)%l1 = [F,F,F,T1,F,T1,T1,F,T1,T1] 	! sort-of Fibonacci
	cafar(:)%l2 = T1
	cafar(1:9:2)%l2 = F 					! odd elements
	cafar(:)%l4 = T1
	cafar([2,4,6,8,10])%l4 = F				! even elements
	cafar(:)%l8 = T1
	cafar(9:1:-2)%l8 = F					! odd elements again

	if (caf%l1 .or. caf%l2 .or. caf%l4 .or. caf%l8) error stop 7
	if (any(cafar(:)%l1 .neqv. [F,F,F,T,F,T,T,F,T,T])) error stop 8
	if (any(cafar(:)%l2 .neqv. [F,T1,F,T1,F,T1,F,T1,F,T1])) error stop 9
	if (any(cafar(:)%l4 .neqv. [T1,F,T1,F,T1,F,T1,F,T1,F])) error stop 10
	if (any(cafar(:)%l8 .neqv. [F,T1,F,T1,F,T1,F,T1,F,T1])) error stop 11

end
