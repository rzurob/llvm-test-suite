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
!                                CHARACTER, LOGICAL: USE mod ONLY: A,B, ....
!*
!*  KEY.or.(S)                 :
!*  TARGET(S)                  :
!* ===================================================================

program module_variables_003a

	use mod001f, ONLY: Ai,Ci,Bi,int02,int04,Ar,Cr,real02,Bch,Dch,char02,Cl,Dl,log08

	implicit none

	integer :: me
	
	me = this_image()
	
	!----Test module variables for scalar: INTEGER-----	
	Ai = 100
	Bi = 50000
	Ci = 60000	
	Di = 70000
	
	int02[1] = 2*100
	int04 = 3*100	
		
	print *, me, Ai, Bi, Ci, int02, int04	
	!---------------------------------------------------
	
	!----Test module variables for scalar: REAL---------
	Ar = 1.0
	Cr = 2.0E0
	Dr = 15.0	
	
	real02[1] = 1.0*0.5
		
	print *, me, Ar,Cr,real02
	!---------------------------------------------------
	
	!----Test module variables for scalar: CHARACTER----
	Bch = 'a'
	Dch = 'd'
	Cch = 'c'
	
	char02[1] = 'b'
	char02[2] = 'c'
	char02[3] = 'd'
		
	print *, me, Bch, Dch, char02
	!---------------------------------------------------
	
	!----Test module variables for scalar: LOGICAL------
	
	log08 = 8 >5	!T
	
	Cl = 6 < 1 !F
	Dl = 7 /= 2 !T
	Al = 1 > 4 !F
		
	print *, me, Cl, Dl, log08
	!---------------------------------------------------
	
	
end program module_variables_003a	
