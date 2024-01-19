! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : September 27, 2010
!* .or.GIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Test module variables for scalar: COMPLEX
!
!*
!*  KEY.or.(S)                 :
!*  TARGET(S)                  :
!* ===================================================================

program module_variables_001b

	use mod001f

	implicit none

	real :: a,b,c,discriminant
	integer :: i,me,num

	me = this_image()
	num = num_images()

	if (me == 1) then

		a = 1.0
		b = 2.0
		c = 5.0
		discriminant = b**2 - 4. * a * c

		comp1[me] = (-b + sqrt(cmplx(discriminant,0.)))/(2.*a)
		comp2[me] = (-b - sqrt(cmplx(discriminant,0.)))/(2.*a)

	end if

	if (me == num) then

		a = 1.0
		b = 5.0
		c = 6.0
		discriminant = b**2 - 4. * a * c

		comp1[me] = (-b + sqrt(cmplx(discriminant,0.)))/(2.*a)
		comp2[me] = (-b - sqrt(cmplx(discriminant,0.)))/(2.*a)

	end if

	print *, me, comp1, comp2

end program module_variables_001b
