!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2015-09-24
!*
!*  PRIMARY FUNCTIONS TESTED   : VALUE(F2008 extension) - dummy argument arrays allowed with value
!*
!*  DESCRIPTION                : testing the extensions to the VALUE attribute
!*                                for an array of real type(default kind)
!*								-passing an array to a subroutine to check that
!*								1. dummy argument is equal to the actual argument
!*								2. actual argument doesn't change
!* =============================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901234567890

implicit none
integer SIZEOFA, doCounter
parameter (SIZEOFA = 10)
logical, external :: precision_r4
real  r1(10),r1_r(10)

r1  =atan(1.0)
r1_r=atan(1.0)

call sub11(r1,10)
	do doCounter=1,SIZEOFA
	if (.not. precision_r4(r1(doCounter),r1_r(doCounter))) error stop 1101
	end do
call sub12(r1)
	do doCounter=1,SIZEOFA
	if (.not. precision_r4(r1(doCounter),r1_r(doCounter))) error stop 1201
	end do
call sub13(r1)
	do doCounter=1,SIZEOFA
	if (.not. precision_r4(r1(doCounter),r1_r(doCounter))) error stop 1301
	end do
call sub14(r1)
	do doCounter=1,SIZEOFA
	if (.not. precision_r4(r1(doCounter),r1_r(doCounter))) error stop 1401
	end do
call sub21(r1,10)
	do doCounter=1,SIZEOFA
	if (.not. precision_r4(r1(doCounter),r1_r(doCounter))) error stop 2101
	end do
call sub22(r1)
	do doCounter=1,SIZEOFA
	if (.not. precision_r4(r1(doCounter),r1_r(doCounter))) error stop 2201
	end do
call sub23(r1)
	do doCounter=1,SIZEOFA
	if (.not. precision_r4(r1(doCounter),r1_r(doCounter))) error stop 2301
	end do
call sub24(r1)
	do doCounter=1,SIZEOFA
	if (.not. precision_r4(r1(doCounter),r1_r(doCounter))) error stop 2401
	end do
call sub31(r1,r1)
	do doCounter=1,SIZEOFA
	if (.not. precision_r4(r1(doCounter),r1_r(doCounter))) error stop 3101
	end do
call sub32(r1,r1)
	do doCounter=1,SIZEOFA
	if (.not. precision_r4(r1(doCounter),r1_r(doCounter))) error stop 3201
	end do

contains

subroutine sub11(arg,n)
    real :: arg(n)
	integer n
	value arg

	do doCounter=1,SIZEOFA
		if (.not. precision_r4(arg(doCounter),r1(doCounter))) error stop 1110
	end do
	if (size(arg) .ne. SIZEOFA) error stop 1111
	if ( any(lbound(arg) .ne. 1)) error stop 1112
	if ( any(ubound(arg) .ne. SIZEOFA)) error stop 1113
	if (rank(arg) .ne. 1) error stop 1114
	if (any(shape(arg) .ne. SIZEOFA)) error stop 1115
	arg=4*atan(1.0)
end subroutine

subroutine sub12(arg)
    real :: arg(10)
	value arg

	do doCounter=1,SIZEOFA
		if (.not. precision_r4(arg(doCounter),r1(doCounter))) error stop 1210
	end do
	if (size(arg) .ne. SIZEOFA) error stop 1211
	if ( any(lbound(arg) .ne. 1)) error stop 1212
	if ( any(ubound(arg) .ne. SIZEOFA)) error stop 1213
	if (rank(arg) .ne. 1) error stop 1214
	if (any(shape(arg) .ne. SIZEOFA)) error stop 1215
	arg=4*atan(1.0)
end subroutine

subroutine sub13(arg)
    real :: arg(SIZEOFA)
	value arg

	do doCounter=1,SIZEOFA
		if (.not. precision_r4(arg(doCounter),r1(doCounter))) error stop 1310
	end do
	if (size(arg) .ne. SIZEOFA) error stop 1311
	if ( any(lbound(arg) .ne. 1)) error stop 1312
	if ( any(ubound(arg) .ne. SIZEOFA)) error stop 1313
	if (rank(arg) .ne. 1) error stop 1314
	if (any(shape(arg) .ne. SIZEOFA)) error stop 1315
	arg=4*atan(1.0)
end subroutine

subroutine sub14(arg)
    real :: arg(:)
	value arg

	do doCounter=1,SIZEOFA
		if (.not. precision_r4(arg(doCounter),r1(doCounter))) error stop 1410
	end do
	if (size(arg) .ne. SIZEOFA) error stop 1411
	if ( any(lbound(arg) .ne. 1)) error stop 1412
	if ( any(ubound(arg) .ne. SIZEOFA)) error stop 1413
	if (rank(arg) .ne. 1) error stop 1414
	if (any(shape(arg) .ne. SIZEOFA)) error stop 1415
	arg=4*atan(1.0)
end subroutine

subroutine sub21(arg,n)
	integer n
	real, DIMENSION(n) :: arg
	value arg

	do doCounter=1,SIZEOFA
		if (.not. precision_r4(arg(doCounter),r1(doCounter))) error stop 2110
	end do
	if (size(arg) .ne. SIZEOFA) error stop 2111
	if ( any(lbound(arg) .ne. 1)) error stop 2112
	if ( any(ubound(arg) .ne. SIZEOFA)) error stop 2113
	if (rank(arg) .ne. 1) error stop 2114
	if (any(shape(arg) .ne. SIZEOFA)) error stop 2115
	arg=4*atan(1.0)
end subroutine

subroutine sub22(arg)
	real, DIMENSION(10) :: arg
	value arg

	do doCounter=1,SIZEOFA
		if (.not. precision_r4(arg(doCounter),r1(doCounter))) error stop 2210
	end do
	if (size(arg) .ne. SIZEOFA) error stop 2211
	if ( any(lbound(arg) .ne. 1)) error stop 2212
	if ( any(ubound(arg) .ne. SIZEOFA)) error stop 2213
	if (rank(arg) .ne. 1) error stop 2214
	if (any(shape(arg) .ne. SIZEOFA)) error stop 2215
	arg=4*atan(1.0)
end subroutine

subroutine sub23(arg)
	real, DIMENSION(SIZEOFA) :: arg
	value arg

	do doCounter=1,SIZEOFA
		if (.not. precision_r4(arg(doCounter),r1(doCounter))) error stop 2310
	end do
	if (size(arg) .ne. SIZEOFA) error stop 2311
	if ( any(lbound(arg) .ne. 1)) error stop 2312
	if ( any(ubound(arg) .ne. SIZEOFA)) error stop 2313
	if (rank(arg) .ne. 1) error stop 2314
	if (any(shape(arg) .ne. SIZEOFA)) error stop 2315
	arg=4*atan(1.0)
end subroutine

subroutine sub24(arg)
	real, DIMENSION(:) :: arg
	value arg

	do doCounter=1,SIZEOFA
		if (.not. precision_r4(arg(doCounter),r1(doCounter))) error stop 2410
	end do
	if (size(arg) .ne. SIZEOFA) error stop 2411
	if ( any(lbound(arg) .ne. 1)) error stop 2412
	if ( any(ubound(arg) .ne. SIZEOFA)) error stop 2413
	if (rank(arg) .ne. 1) error stop 2414
	if (any(shape(arg) .ne. SIZEOFA)) error stop 2415
	arg=4*atan(1.0)
end subroutine

subroutine sub31(arg,arg2)
	real arg2(:)
	real arg(size(arg2))
	value arg

	do doCounter=1,SIZEOFA
		if (.not. precision_r4(arg(doCounter),r1(doCounter))) error stop 3110
	end do
	if (size(arg) .ne. SIZEOFA) error stop 3111
	if ( any(lbound(arg) .ne. 1)) error stop 3112
	if ( any(ubound(arg) .ne. SIZEOFA)) error stop 3113
	if (rank(arg) .ne. 1) error stop 3114
	if (any(shape(arg) .ne. SIZEOFA)) error stop 3115
	arg=4*atan(1.0)
end subroutine

subroutine sub32(arg,arg2)
	real arg2(:)
	real , DIMENSION (size(arg2)) :: arg
	value arg

	do doCounter=1,SIZEOFA
		if (.not. precision_r4(arg(doCounter),r1(doCounter))) error stop 3210
	end do
	if (size(arg) .ne. SIZEOFA) error stop 3211
	if ( any(lbound(arg) .ne. 1)) error stop 3212
	if ( any(ubound(arg) .ne. SIZEOFA)) error stop 3213
	if (rank(arg) .ne. 1) error stop 3214
	if (any(shape(arg) .ne. SIZEOFA)) error stop 3215
	arg=4*atan(1.0)
end subroutine

end