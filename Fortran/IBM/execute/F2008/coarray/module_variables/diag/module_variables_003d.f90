! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Denis Navotniy
!*  DATE                       : October 29, 2010
!* .or.GIN                     :
!*                             : 
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              :
!*
!*  DESCRIPTION                : Test that if module (to share function) was compilled with
!*								 -qnocaf and main program was compiled with -qcaf, compiler  will
!*								 generte error message
!*
!*  KEY.or.(S)                 :
!*  TARGET(S)                  :
!* ===================================================================

program module_variables_003d
	
	use module_003d
	
	implicit none
	integer :: j = 15
	integer :: i = 45
	
	print *, i,'+',j,'=',sum(i,j)
	
	
	
end program module_variables_003d