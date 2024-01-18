! *********************************************************************
!*  ===================================================================
!*  F2008 Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            : POPPAR_003f
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
!*  DESCRIPTION                : Test that POPPAR can be used
!*			         as constant expression in an
!*				 initialization expressioan (in function)
!*
!*
!*  TARGET(S)                  :
!* ===================================================================
program POPPAR_003f
	
	implicit none
	
	integer(1) :: res1, res1a
	integer(1) :: res2 ,res2a
	integer(1) :: res3, res3a
	integer :: SumPOPPAR_external
	
	res1 = SumPOPPAR_internal(1,10)
	res2 = SumPOPPAR_internal(POPPAR(1),POPPAR(10))
	res3 = SumPOPPAR_internal(2,POPPAR(10))
	
	if(res1 /= 1) ERROR STOP 1
	if(res2 /= 1) ERROR STOP 2
	if(res3 /= 1) ERROR STOP 3
	
	res1a = SumPOPPAR_external(1,10)
	res2a = SumPOPPAR_external(POPPAR(1),POPPAR(10))
	res3a = SumPOPPAR_external(2,POPPAR(10))
	
	if(res1a /= 1) ERROR STOP 4
	if(res2a /= 1) ERROR STOP 5
	if(res3a /= 1) ERROR STOP 6
	
	contains
	
	integer function SumPOPPAR_internal(i,j)
		
		integer :: i, j
		integer :: i1, j1
		
		i1 = POPPAR(i)
		j1 = POPPAR(j)
		
		SumPOPPAR_internal = i1+j1
		
		
	end function SumPOPPAR_internal
		
		
end program POPPAR_003f

integer function SumPOPPAR_external(i,j)
		
		integer :: i, j
		integer :: i1, j1
		
		i1 = POPPAR(i)
		j1 = POPPAR(j)
		
		SumPOPPAR_external = i1+j1
		
		
end function SumPOPPAR_external
