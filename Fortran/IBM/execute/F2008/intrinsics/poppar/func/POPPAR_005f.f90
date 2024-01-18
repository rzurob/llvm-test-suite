! *********************************************************************
!*  ===================================================================
!*  F2008 Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            : POPPAR_005f
!*
!*  PROGRAMMER                 : Denis Navotniy
!*  DATE                       : November 30, 2010
!*
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              :
!*
!*  DESCRIPTION                : Test that POPPAR can be used as 
!*				 constant expression in an 
!*				 initialization expressioan (in module)
!*
!*
!*  TARGET(S)                  :
!* ===================================================================
program POPPAR_005f
	
	use modulePOPPAR
	
	implicit none
	
	integer(1), parameter :: res1 = i1
	integer(1), parameter :: res2 = i2
	integer(1), parameter :: res3 = i3
	integer(1), parameter :: res4 = i4
	integer :: res5
	integer :: res6
	integer :: res7
	integer :: res8
	integer :: res9
	integer :: res10
	
	if(res1 /= 1) ERROR STOP 1
	if(res2 /= 0) ERROR STOP 2
	if(res3 /= 1) ERROR STOP 3
	if(res4 /= 0) ERROR STOP 3
	
	res5 = SumPOPPAR(1,10)
	res6 = SumPOPPAR(POPPAR(1),POPPAR(10))
	res7 = SumPOPPAR(2,POPPAR(10))
	
	if(res5 /= 1) ERROR STOP 4
	if(res6 /= 1) ERROR STOP 5
	if(res7 /= 1) ERROR STOP 6
	
	call Sum(1,10)
	res8 = res
	
	call Sum(POPPAR(1),POPPAR(10))
	res9 = res
	
	call Sum(2,POPPAR(10))
	res10 = res
	
	if(res8 /= 1) ERROR STOP 7
	if(res9 /= 1) ERROR STOP 8
	if(res10 /= 1) ERROR STOP 9
	

	
end program POPPAR_005f
