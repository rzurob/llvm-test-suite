!234567890123456789012345678901234567890123456789012345678901234567890
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : codimension_d03.f
!*
!*  PROGRAMMER                 : Francesco Cassullo
!*  DATE                       : October 2010
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Coarray
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Test exceeding corank/rank limitations via the
!*                               attribute statements.
!*                            
!234567890123456789012345678901234567890123456789012345678901234567890

program main

	integer, save :: caf0, caf1
	integer, save :: caf2(1,2,3,4,5,6,7,8,9)
	integer, save :: caf3[*]
	integer, save, dimension(1,2,3,4,5) :: caf4
	
	
	codimension caf0[1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,20,*]
	codimension caf1[1,2,3,4,5,6,7,8,9,*]
	dimension caf1(1,2,3,4,5,6,7,8,9,10,11)
	
	codimension caf2[1,2,3,4,5,6,7,8,9,10,11,*]
	dimension caf3(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)

	codimension caf4[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,*]
	
end
