! *********************************************************************
!*  ===================================================================
!*  F2008 Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            : POPPAR_001f
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
!*  DESCRIPTION                : Test that POPPAR can be used as
!*				 constant expression in an
!*				 initialization expressioan 
!*
!*  TARGET(S)                  :
!* ===================================================================

program POPPAR_001f
	
	implicit none
	
	integer(1), parameter :: i1=1
	integer(2), parameter :: i2=10
	integer(4), parameter :: i4=100
	integer(8), parameter :: i8=1000

	integer(1), parameter :: res1=POPPAR(i1)
	integer(1), parameter :: res2=POPPAR(i2)
	integer(1), parameter :: res4=POPPAR(i4)
	integer(1), parameter :: res8=POPPAR(i8)	

	if(res1 /= 1) ERROR STOP 1
	if(res2 /= 0) ERROR STOP 2
	if(res4 /= 1) ERROR STOP 4
	if(res8 /= 0) ERROR STOP 8

	
end program POPPAR_001f
