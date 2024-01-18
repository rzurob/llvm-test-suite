! *********************************************************************
!*  ===================================================================
!*  F2008 Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            : POPCNT_005f
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
!*  DESCRIPTION                : Test that POPCNT can be used as 
!*				 constant expression in an 
!*				 initialization expressioan (in module)
!*
!*
!*  TARGET(S)                  :
!* ===================================================================
program POPCNT_005f
	
	use modulePOPCNT
	
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
	if(res2 /= 2) ERROR STOP 2
	if(res3 /= 3) ERROR STOP 3
	if(res4 /= 6) ERROR STOP 3
	
	res5 = SumPOPCNT(1,10)
	res6 = SumPOPCNT(POPCNT(1),POPCNT(10))
	res7 = SumPOPCNT(2,POPCNT(10))
	
	if(res5 /= 3) ERROR STOP 4
	if(res6 /= 2) ERROR STOP 5
	if(res7 /= 2) ERROR STOP 6
	
	call Sum(1,10)
	res8 = res
	
	call Sum(POPCNT(1),POPCNT(10))
	res9 = res
	
	call Sum(2,POPCNT(10))
	res10 = res
	
	if(res8 /= 3) ERROR STOP 7
	if(res9 /= 2) ERROR STOP 8
	if(res10 /= 2) ERROR STOP 9
	

	
end program POPCNT_005f
