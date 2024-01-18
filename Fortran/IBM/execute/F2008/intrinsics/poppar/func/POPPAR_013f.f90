! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : December 06, 2010
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Test array constructors for the
!*				 POPPAR() arguments
!*
!*  TARGET(S)                  :
!* ===================================================================
program POPPAR_013f

	implicit none

	integer :: i, j
	integer, dimension(10) :: arr1, res1, res1a, verify1
	integer, dimension(10) :: arr2, res2, res2a, verify2
	integer, dimension(15) :: arr3, res3, res3a, verify3
	integer, dimension(5)  :: arr4, res4, res4a, verify4
	integer, dimension(12) :: arr5, res5, res5a, verify5
	integer, dimension(10) :: arr6, res6, res6a, verify6
	integer, dimension(10) :: arr7, res7, res7a, verify7

	arr1	=	(/1,2,3,4,5,6,7,8,9,10/)
	verify1	=	(/1,1,0,1,0,0,1,1,0,0/)
	res1	=	POPPAR(arr1)
	res1a	=	POPPAR((/1,2,3,4,5,6,7,8,9,10/))

	if ( any(res1 /= res1a) ) ERROR STOP 1
	if ( any(res1 /= verify1) ) ERROR STOP 2
	if ( any(res1a /= verify1) ) ERROR STOP 3

	arr2	=	(/(i,i=1,10)/)
	verify2	=	(/1,1,0,1,0,0,1,1,0,0/)
	res2	=	POPPAR(arr2)
	res2a	=	POPPAR((/(i,i=1,10)/))

	if ( any(res2 /= res2a) ) ERROR STOP 4
	if ( any(res2 /= verify2) ) ERROR STOP 5
	if ( any(res2a /= verify2) ) ERROR STOP 6

	arr3	=	(/((i,i=1,5), j=1,3)/)
	verify3	=	(/1,1,0,1,0,1,1,0,1,0,1,1,0,1,0/)
	res3		=	POPPAR(arr3)
	res3a	=	POPPAR((/((i,i=1,5), j=1,3)/))

	if ( any(res3 /= res3a) ) ERROR STOP 7
	if ( any(res3 /= verify3) ) ERROR STOP 8
	if (	 any(res3a /= verify3) ) ERROR STOP 9

	arr4	=	(/(i,i=1,15,3)/)
	verify4	=	(/1,1,1,0,1/)
	res4	=	POPPAR(arr4)
	res4a	=	POPPAR((/(i,i=1,15,3)/))

	if ( any(res4 /= res4a) ) ERROR STOP 10
	if ( any(res4 /= verify4) ) ERROR STOP 11
	if ( any(res4a /= verify4) ) ERROR STOP 12

	arr5	=	(/(i,i+2,i+4, i=1,4)/)
	verify5	=	(/1,0,0,1,1,0,0,0,1,1,0,1/)
	res5	=	POPPAR(arr5)
	res5a	=	POPPAR((/(i,i+2,i+4, i=1,4)/))

	if ( any(res5 /= res5a) ) ERROR STOP 13
	if ( any(res5 /= verify5) ) ERROR STOP 14
	if ( any(res5a /= verify5) ) ERROR STOP 15

	arr6	= (/( (i,i=1,j), j=1,4 )/)
	verify6	=	(/1,1,1,1,1,0,1,1,0,1/)
	res6	=	POPPAR(arr6)
	res6a	=	POPPAR((/( (i,i=1,j), j=1,4 )/))

	if ( any(res6 /= res6a) ) ERROR STOP 16
	if ( any(res6 /= verify6) ) ERROR STOP 17
	if ( any(res6a /= verify6) ) ERROR STOP 18

	arr7	=	(/1,2,(i,i+1,i=3,7,2),9,10/)
	verify7	=	(/1,1,0,1,0,0,1,1,0,0/)
	res7	=	POPPAR(arr7)
	res7a	=	POPPAR((/1,2,(i,i+1,i=3,7,2),9,10/))

	if ( any(res7 /= res7a) ) ERROR STOP 19
	if ( any(res7 /= verify7) ) ERROR STOP 20
	if ( any(res7a /= verify7) ) ERROR STOP 21

end program POPPAR_013f
