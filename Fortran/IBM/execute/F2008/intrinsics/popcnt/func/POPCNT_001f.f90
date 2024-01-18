! *********************************************************************
!*  ===================================================================
!*  F2008 Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            : POPCNT_001f
!*
!*  PROGRAMMER                 : Denis Navotniy
!*  DATE                       : November 16, 2010
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
!*				 initialization expressioan 
!*
!*
!*  TARGET(S)                  :
!* ===================================================================

program POPCNT_001f
	
	implicit none
	
	integer(1), parameter :: i1=1
	integer(2), parameter :: i2=10
	integer(4), parameter :: i4=100
	integer(8), parameter :: i8=1000

	integer(1), parameter :: res1=POPCNT(i1)
	integer(1), parameter :: res2=POPCNT(i2)
	integer(1), parameter :: res4=POPCNT(i4)
	integer(1), parameter :: res8=POPCNT(i8)	
		
	if(res1 /= 1) ERROR STOP 1
	if(res2 /= 2) ERROR STOP 2
	if(res4 /= 3) ERROR STOP 4
	if(res8 /= 6) ERROR STOP 8

	
end program POPCNT_001f
