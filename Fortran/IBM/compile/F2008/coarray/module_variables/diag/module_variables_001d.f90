! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : October 29, 2010
!* .or.GIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Test that if module (to share integer variable) was compilled with
!*								 -qnocaf and main program was compiled with -qcaf, compiler  will
!*								 generte error message
!*
!*  KEY.or.(S)                 :
!*  TARGET(S)                  :
!* ===================================================================

program module_variables_001d

	use module_001d

	i = 12;

	print *,'i=',i, 'Pi=',Pi

end program module_variables_001d