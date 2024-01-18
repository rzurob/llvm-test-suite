!234567890123456789012345678901234567890123456789012345678901234567890
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : codimension_d02.f
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
!*  DESCRIPTION                : Test exceeding corank/rank limitations via
!*                               attributes and traditional bracket declarations.
!*                            
!234567890123456789012345678901234567890123456789012345678901234567890

program main

	integer, save :: caf0[1]
	integer, save :: caf1[1,0,1,*]
	
	integer, save :: caf2[1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,20,*]
	integer, save :: caf3(1,2,3,4,5,6,7,8,9,10)[1,2,3,4,5,6,7,8,9,10,*]
	
	integer, save, dimension(1,2,3,4,5) :: caf4[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,*]
	integer, save, codimension[1,2,3,4,5,6,7,8,*] :: caf5(1,2,3,4,5,6,7,8,9,10,11,12)
	integer, save, codimension[1,2,3,4,5,6,7,8,9,10,*], dimension(1,2,3,4,5,6,7,8,9,10) :: caf6

end
