! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : September 27, 2010
!* .or.GIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Test module variables for scalar: INTEGER, REAL,
!                                CHARACTER, LOGICAL
!*
!*  KEY.or.(S)                 :
!*  TARGET(S)                  :
!* ===================================================================

program module_variables_001a

	use mod001f

	implicit none

	integer :: me

	me = this_image()

	!----Test module variables for scalar: INTEGER-----
	int02 = 100
	int04 = 50000
	int08 = 800000000

	int02[1] = 2*100
	int04[1] = 50000+10000
	int08[1] = 800000000/2

	print *, me, int02, int04, int08
	!---------------------------------------------------

	!----Test module variables for scalar: REAL---------
	real02 = 1.0
	real04 = 2.0E0
	real08 = 3.0D0

	real02[1] = 1.0*0.5
	real04[1] = 2.0E0 + 1.5E0
	real08[1] = 3.0D0/2.0

	print *, me, real02, real04, real08
	!---------------------------------------------------

	!----Test module variables for scalar: CHARACTER----
	char02 = 'a'

	char02[1] = 'b'
	char02[1] = 'c'
	char02[1] = 'd'

	print *, me, char02
	!---------------------------------------------------

	!----Test module variables for scalar: LOGICAL------
	log02 = 6 > 1 !T
	log04 = 7 == 2 !F
	log08 = 8 >5	!T

	log02[1] = 6 < 1 !F
	log04[1] = 7 /= 2 !T
	log08[1] = 8 < 5 !F

	print *, me, log02, log04, log08
	!---------------------------------------------------

end program module_variables_001a
