!234567890123456789012345678901234567890123456789012345678901234567890
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : datastmnt_d01.f
!*
!*  PROGRAMMER                 : Francesco Cassullo
!*  DATE                       : November 2010
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Coarray
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : The same coarray variable cannot be in multiple DATA statements.
!*                            
!234567890123456789012345678901234567890123456789012345678901234567890

program main
	integer, save :: caf0[*]
	integer, save :: caf1(2)[1:*]
	
	data caf0/95/, caf0/10/
	data caf1/2*3/
	data caf1/2*100/
end
