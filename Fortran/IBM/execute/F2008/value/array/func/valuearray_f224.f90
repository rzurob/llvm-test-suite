!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : F2008/value/array/func/valuearray_f225.f
!*
!*  PROGRAMMER                 : Cezar Lutac 
!*  DATE                       : 2015-09-24
!*
!*  PRIMARY FUNCTIONS TESTED   : VALUE(F2008 extension) - dummy argument arrays allowed with value
!*
!*  DESCRIPTION                : testing the extensions to the VALUE attribute
!*                         		for passing array sections of real and imaginary components of complex arrays
!*									testing will check that
!*								1. dummy argument is equal to the actual argument
!*								2. actual argument doesn't change
!* =============================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901234567890

implicit none

integer SIZEOFA, doCounter
parameter (SIZEOFA = 10)

logical, external :: precision_x8, precision_r4

complex*8  com1(10),com1_r(10)


com1  =(atan(1.0),2*atan(1.0))
com1_r=(atan(1.0),2*atan(1.0))

call sub1_real(real(com1))
	do doCounter=1,SIZEOFA
		if (.not. precision_r4(com1(doCounter),com1_r(doCounter))) error stop 11
	end do

call sub1_imag(aimag(com1))
	do doCounter=1,SIZEOFA
		if (.not. precision_r4(com1(doCounter),com1_r(doCounter))) error stop 12
	end do	

call sub1_all(com1)	
	do doCounter=1,SIZEOFA
		if (.not. precision_r4(com1(doCounter),com1_r(doCounter))) error stop 13
	end do	
	
contains

subroutine sub1_real(arg)
    real :: arg(:)
	value arg
	do doCounter=1,size(arg)
		if (.not. precision_r4(arg(doCounter),real(com1(doCounter)))) error stop 110
	end do	
			
	if (size(arg) .ne. SIZEOFA) 			error stop 111
	if ( any(lbound(arg) .ne. 1)) 	error stop 112
	if ( any(ubound(arg) .ne. SIZEOFA)) 	error stop 113
	if (rank(arg) .ne. 1) 			error stop 114
	if (any(shape(arg) .ne. SIZEOFA)) 	error stop 115	
	
	arg=3*atan(1.0)
end subroutine

subroutine sub1_imag(arg)
    real :: arg(:)
	value arg
	do doCounter=1,size(arg)
		if (.not. precision_r4(arg(doCounter),aimag(com1(doCounter)))) error stop 210
	end do	
			
	if (size(arg) .ne. SIZEOFA) 			error stop 211
	if ( any(lbound(arg) .ne. 1)) 	error stop 212
	if ( any(ubound(arg) .ne. SIZEOFA)) 	error stop 213
	if (rank(arg) .ne. 1) 			error stop 214
	if (any(shape(arg) .ne. SIZEOFA)) 	error stop 215	
	arg=3*atan(1.0)
end subroutine

subroutine sub1_all(arg)
    complex*8 :: arg(:)
	value arg
	do doCounter=1,size(arg)
		if (.not. precision_r4(arg(doCounter)%re,real(com1(doCounter)))) error stop 3101
		if (.not. precision_r4(arg(doCounter)%im,aimag(com1(doCounter)))) error stop 3102
	end do	
	
	if (size(arg) .ne. SIZEOFA) 	error stop 311
	if ( any(lbound(arg) .ne. 1)) 	error stop 312
	if ( any(ubound(arg) .ne. SIZEOFA)) 	error stop 313
	if (rank(arg) .ne. 1) 			error stop 314
	if (any(shape(arg) .ne. SIZEOFA)) 	error stop 315	
	arg=(3*atan(1.0),7*atan(1.0))
end subroutine	

end
