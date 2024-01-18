! *********************************************************************
!*  ===================================================================
!*  F2008 Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            : POPPAR_011f
!*
!*  PROGRAMMER                 : Denis Navotniy
!*  DATE                       : December 07, 2010
!*
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              :
!*
!*  DESCRIPTION                : Test that POPPAR() can get argumen
!*				 POPCNT() or POPPAR() functions
!*				(ex. POPCNT(POPPAR(POPCNT(arr))) )
!*
!*
!*  TARGET(S)                  :
!* ===================================================================
program POPPAR_011f
	
	implicit none
	integer :: i
	
	integer :: a=10, b=-15
	integer :: res1, res1a
	integer, dimension(5) :: arr1, res1b
	
	
	res1=POPPAR(POPPAR(a))
	res1a=POPPAR(POPPAR(b))
	arr1=[1,2,3,4,5]
	res1b=POPPAR(POPPAR(arr1))
	
	
	if (res1 /= 0) ERROR STOP 1
	if (res1a /= 1) ERROR STOP 2
	if (any(res1b /= [1,1,0,1,0])) ERROR STOP 3
	
	res1=POPPAR(POPPAR(POPCNT(a)))
	res1a=POPPAR(POPPAR(POPCNT(b)))
	arr1=[1,2,3,4,5]
	res1b=POPPAR(POPPAR(POPCNT(arr1)))
	
	if (res1 /= 1) ERROR STOP 4
	if (res1a /= 0) ERROR STOP 5
	if (any(res1b /= [1,1,1,1,1])) ERROR STOP 6
	
	res1=POPPAR(POPCNT(POPPAR(a)))
	res1a=POPPAR(POPCNT(POPPAR(b)))
	arr1=[1,2,3,4,5]
	res1b=POPPAR(POPCNT(POPPAR(arr1)))
	
	if (res1 /= 0) ERROR STOP 7
	if (res1a /= 1) ERROR STOP 8
	if (any(res1b /= [1,1,0,1,0])) ERROR STOP 9
			
end program POPPAR_011f
