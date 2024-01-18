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
!*  DESCRIPTION                : Test that if module (to share subroutine) was compilled with
!*								 -qnocaf and main program was compiled with -qcaf, compiler  will
!*								 generte error message
!*
!*  KEY.or.(S)                 :
!*  TARGET(S)                  :
!* ===================================================================

program module_variables_002d
	
	use module_002d
	
	implicit none
	integer :: j = 15
	call pr(j)
	
	
	
end program module_variables_002d