! *********************************************************************
!*  ===================================================================
!*  F2008 Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            : POPPAR_005d
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
!*  DESCRIPTION                : Test that invoking POPPAR() with constant 
!*				 expression should work as expression or
!* 				 initial value is evaluated at compile time.
!*
!*
!*  TARGET(S)                  :
!* ===================================================================

program POPPAR_005d
	
	implicit none
	
	integer, parameter :: const=16
	integer(POPPAR(const)) :: i=10
	
	print *, POPPAR(i) ! 


end program POPPAR_005d
