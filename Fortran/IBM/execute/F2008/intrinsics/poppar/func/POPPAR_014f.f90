! *********************************************************************
!*  ===================================================================
!*  F2008 Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            : POPPAR_014f
!*
!*  PROGRAMMER                 : Denis Navotniy
!*  DATE                       : December 06, 2010
!*
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              :
!*
!*  DESCRIPTION                : Test different type of integer 
!*				 argument in POPPAR()
!*
!*
!*  TARGET(S)                  :
!* ===================================================================
program POPPAR_014f
	
	implicit none
	
	integer, parameter :: int_kind2=2
	integer, parameter :: int_kind4=4
	integer, parameter :: int_kind8=8
		
	integer(kind=int_kind2) :: int02=11
	integer(kind=int_kind4) :: int04=10000000
	integer(kind=int_kind8) :: int08=2000000000
		
	integer :: res1, res2, res3
	integer :: verify1, verify2, verify3
	
	res1=POPPAR(int02)
	res2=POPPAR(int04)
	res3=POPPAR(int08)
	
	!print *, res1, res2, res3
			
	if(res1 /= 1) ERROR STOP 1
	if(res2 /= 0) ERROR STOP 2
	if(res3 /= 1) ERROR STOP 3
			
end program POPPAR_014f
