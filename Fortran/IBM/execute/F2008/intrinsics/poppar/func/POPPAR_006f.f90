! *********************************************************************
!*  ===================================================================
!*  F2008 Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            : POPPAR_006f
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
!*  DESCRIPTION                : Test that array argument in POPPAR() may 
!*				 have different dimension or size, and also 
!*				 assumed shape/size array dummy argument as
!*								 argument of these intrinsics
!*
!*
!*  TARGET(S)                  :
!* ===================================================================
program POPPAR_006f
	
	implicit none
	
	integer, dimension(6) :: arr
	integer, allocatable, dimension(:) :: arr0, arr1
	integer :: res1
	integer :: res2
	integer :: res3
	integer :: res4
	integer :: res5 ,res6
	integer :: s
	
	arr = POPPAR([1,2,4,8,16,32])
	s = size(arr)
	
	arr0=[1,2,4,8,16,32]
	arr1=POPPAR(arr0)
	res5 = size(arr1)
		
	arr0=[1,2,4,8,16,32,64]
	arr1=POPPAR(arr0)
	res6 = size(arr1)
	
	res1 = sum(arr) 
	res2 = sum(POPPAR([1,2,4,8,16,32]))
	res3 = PRODUCT(arr)
	res4 = PRODUCT(POPPAR([1,2,4,8,16,32]))
	
	if(res1 /= s) ERROR STOP 1
	if(res2 /= s) ERROR STOP 2
	if(res3 /= 1) ERROR STOP 3
	if(res4 /= 1) ERROR STOP 4
	if(res5 == res6) ERROR STOP 5
	
	call sub1(POPPAR(arr))

	contains
	
	subroutine sub1(x)
		
		integer, dimension(-1:*) :: x
			
		if (size(POPPAR(x(-1:4))) /= 6) ERROR STOP 6
			
	end subroutine sub1
	
end program POPPAR_006f
