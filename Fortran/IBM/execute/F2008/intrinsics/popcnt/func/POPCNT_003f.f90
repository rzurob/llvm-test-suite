! *********************************************************************
!*  ===================================================================
!*  F2008 Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            : POPCNT_003f
!*
!*  PROGRAMMER                 : Denis Navotniy
!*  DATE                       : November 29, 2010
!*
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              :
!*
!*  DESCRIPTION                : Test that POPCNT can be used
!*				 as constant expression in an
!*				 initialization expressioan (in function)
!*
!*
!*  TARGET(S)                  :
!* ===================================================================
program POPCNT_003f
	
	implicit none
	
	integer(1) :: res1, res1a
	integer(1) :: res2 ,res2a
	integer(1) :: res3, res3a
	integer :: SumPOPCNT_external
		
	res1 = SumPOPCNT_internal(1,10)
	res2 = SumPOPCNT_internal(POPCNT(1),POPCNT(10))
	res3 = SumPOPCNT_internal(2,POPCNT(10))
	
	if(res1 /= 3) ERROR STOP 1
	if(res2 /= 2) ERROR STOP 2
	if(res3 /= 2) ERROR STOP 3
	
	res1a = SumPOPCNT_external(1,10)
	res2a = SumPOPCNT_external(POPCNT(1),POPCNT(10))
	res3a = SumPOPCNT_external(2,POPCNT(10))
	
	if(res1a /= 3) ERROR STOP 4
	if(res2a /= 2) ERROR STOP 5
	if(res3a /= 2) ERROR STOP 6
	
	contains
	
	integer function SumPOPCNT_internal(i,j)
		
		integer :: i, j
		integer :: i1, j1
		
		i1 = POPCNT(i)
		j1 = POPCNT(j)
		
		SumPOPCNT_internal = i1+j1
		
		
	end function SumPOPCNT_internal
		
		
end program POPCNT_003f

integer function SumPOPCNT_external(i,j)
		
		integer :: i, j
		integer :: i1, j1
		
		i1 = POPCNT(i)
		j1 = POPCNT(j)
		
		SumPOPCNT_external = i1+j1
		
		
end function SumPOPCNT_external
