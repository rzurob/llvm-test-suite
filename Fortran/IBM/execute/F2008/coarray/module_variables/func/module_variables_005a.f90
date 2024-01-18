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
!*  DESCRIPTION                : Test coarray module variables for scalar: INTEGER, REAL,
!                                CHARACTER, LOGICAL: USE module, local_name => module_name
!*
!*  KEY.or.(S)                 :
!*  TARGET(S)                  :
!* ===================================================================

program module_variables_004a

	use mod001f, ONLY: Ai_new => Ai, int02_new => int02, Ar_new => Ar, real02_new => real02, Dch_new => Dch, char02_new => char02, log08_new => log08,  Cl_new => Cl

	implicit none

	integer :: me
	
	me = this_image()
	
	!----Test module variables for scalar: INTEGER-----	
	Ai_new = 100
		
	int02_new[1] = 2*100
	
	print *, me, Ai_new, int02_new
	!---------------------------------------------------
	
	!----Test module variables for scalar: REAL---------
	Ar_new = 1.0
			
	real02_new[1] = 1.0*0.5
		
	print *, me, Ar_new,real02_new
	!---------------------------------------------------
	
	!----Test module variables for scalar: CHARACTER----
	Dch_new = 'd'
	
	char02_new[2] = 'c'
	
	print *, me, Dch_new, char02_new
	!---------------------------------------------------
	
	!----Test module variables for scalar: LOGICAL------
	
	log08_new = 8 >5	!T
	
	Cl_new = 6 < 1 !F
			
	print *, me, Cl_new, log08_new
	!---------------------------------------------------
	
	
end program module_variables_004a	
