!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : F2008/value/array/func/valuearray_f151.f
!*
!*  DATE                       : 2015-09-24
!*
!*  PRIMARY FUNCTIONS TESTED   : VALUE(F2008 extension) - dummy argument arrays allowed with value
!*
!*  DESCRIPTION                : testing the extensions to the VALUE attribute
!*                                for an array of a derived type:
!*								- the derived type tested has only intrinsic types
!*								- the derived type also has the SEQUENCE keyword
!*								-passing an array to a subroutine to check that
!*								1. dummy argument is equal to the actual argument
!*								2. actual argument doesn't change
!* =============================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901234567890

implicit none

type t1
  sequence
  integer i1
  real r1
  logical l1
  complex c1
  character(10) char1
end type

integer SIZEOFA,doCounter
parameter (SIZEOFA = 10)
logical, external :: precision_r4
logical, external :: precision_x8
type(t1) dvt1(10),dvt1_r(10)


dvt1 	= t1(100,atan(1.0)	, .true.,(2*atan(1.0),3*atan(1.0)),"1a3b5c7d9e")
dvt1_r 	= t1(100,atan(1.0)	, .true.,(2*atan(1.0),3*atan(1.0)),"1a3b5c7d9e")

call sub11(dvt1,10)
do doCounter=1,SIZEOFA
	if (dvt1(doCounter)%i1 		.ne. 	dvt1_r(doCounter)%i1) 			error stop 11011
	if (.not. precision_r4 (dvt1(doCounter)%r1,dvt1_r(doCounter)%r1)) 	error stop 11012
	if (dvt1(doCounter)%l1 		.NEQV. 	dvt1_r(doCounter)%l1) 			error stop 11013
	if (.not. precision_x8 (dvt1(doCounter)%c1,dvt1_r(doCounter)%c1)) 	error stop 11014
	if (dvt1(doCounter)%char1 	.ne. 	dvt1_r(doCounter)%char1) 		error stop 11015
end do
call sub12(dvt1)
do doCounter=1,SIZEOFA
	if (dvt1(doCounter)%i1 		.ne. 	dvt1_r(doCounter)%i1) 			error stop 12011
	if (.not. precision_r4 (dvt1(doCounter)%r1,dvt1_r(doCounter)%r1)) 	error stop 12012
	if (dvt1(doCounter)%l1 		.NEQV. 	dvt1_r(doCounter)%l1) 			error stop 12013
	if (.not. precision_x8 (dvt1(doCounter)%c1,dvt1_r(doCounter)%c1)) 	error stop 12014
	if (dvt1(doCounter)%char1 	.ne. 	dvt1_r(doCounter)%char1) 		error stop 12015
end do
call sub13(dvt1)
do doCounter=1,SIZEOFA
	if (dvt1(doCounter)%i1 		.ne. 	dvt1_r(doCounter)%i1) 			error stop 13011
	if (.not. precision_r4 (dvt1(doCounter)%r1,dvt1_r(doCounter)%r1)) 	error stop 13012
	if (dvt1(doCounter)%l1 		.NEQV. 	dvt1_r(doCounter)%l1) 			error stop 13013
	if (.not. precision_x8 (dvt1(doCounter)%c1,dvt1_r(doCounter)%c1)) 	error stop 13014
	if (dvt1(doCounter)%char1 	.ne. 	dvt1_r(doCounter)%char1) 		error stop 13015
end do
call sub14(dvt1)
do doCounter=1,SIZEOFA
	if (dvt1(doCounter)%i1 		.ne. 	dvt1_r(doCounter)%i1) 			error stop 14011
	if (.not. precision_r4 (dvt1(doCounter)%r1,dvt1_r(doCounter)%r1)) 	error stop 14012
	if (dvt1(doCounter)%l1 		.NEQV. 	dvt1_r(doCounter)%l1) 			error stop 14013
	if (.not. precision_x8 (dvt1(doCounter)%c1,dvt1_r(doCounter)%c1)) 	error stop 14014
	if (dvt1(doCounter)%char1 	.ne. 	dvt1_r(doCounter)%char1) 		error stop 14015
end do
call sub21(dvt1,10)
do doCounter=1,SIZEOFA
	if (dvt1(doCounter)%i1 		.ne. 	dvt1_r(doCounter)%i1) 			error stop 21011
	if (.not. precision_r4 (dvt1(doCounter)%r1,dvt1_r(doCounter)%r1)) 	error stop 21012
	if (dvt1(doCounter)%l1 		.NEQV. 	dvt1_r(doCounter)%l1) 			error stop 21013
	if (.not. precision_x8 (dvt1(doCounter)%c1,dvt1_r(doCounter)%c1)) 	error stop 21014
	if (dvt1(doCounter)%char1 	.ne. 	dvt1_r(doCounter)%char1) 		error stop 21015
end do
call sub22(dvt1)
do doCounter=1,SIZEOFA
	if (dvt1(doCounter)%i1 		.ne. 	dvt1_r(doCounter)%i1) 			error stop 22011
	if (.not. precision_r4 (dvt1(doCounter)%r1,dvt1_r(doCounter)%r1)) 	error stop 22012
	if (dvt1(doCounter)%l1 		.NEQV. 	dvt1_r(doCounter)%l1) 			error stop 22013
	if (.not. precision_x8 (dvt1(doCounter)%c1,dvt1_r(doCounter)%c1)) 	error stop 22014
	if (dvt1(doCounter)%char1 	.ne. 	dvt1_r(doCounter)%char1) 		error stop 22015
end do
call sub23(dvt1)
do doCounter=1,SIZEOFA
	if (dvt1(doCounter)%i1 		.ne. 	dvt1_r(doCounter)%i1) 			error stop 23011
	if (.not. precision_r4 (dvt1(doCounter)%r1,dvt1_r(doCounter)%r1)) 	error stop 23012
	if (dvt1(doCounter)%l1 		.NEQV. 	dvt1_r(doCounter)%l1) 			error stop 23013
	if (.not. precision_x8 (dvt1(doCounter)%c1,dvt1_r(doCounter)%c1)) 	error stop 23014
	if (dvt1(doCounter)%char1 	.ne. 	dvt1_r(doCounter)%char1) 		error stop 23015
end do
call sub24(dvt1)
do doCounter=1,SIZEOFA
	if (dvt1(doCounter)%i1 		.ne. 	dvt1_r(doCounter)%i1) 			error stop 24011
	if (.not. precision_r4 (dvt1(doCounter)%r1,dvt1_r(doCounter)%r1)) 	error stop 24012
	if (dvt1(doCounter)%l1 		.NEQV. 	dvt1_r(doCounter)%l1) 			error stop 24013
	if (.not. precision_x8 (dvt1(doCounter)%c1,dvt1_r(doCounter)%c1)) 	error stop 24014
	if (dvt1(doCounter)%char1 	.ne. 	dvt1_r(doCounter)%char1) 		error stop 24015
end do
call sub31(dvt1,dvt1)
do doCounter=1,SIZEOFA
	if (dvt1(doCounter)%i1 		.ne. 	dvt1_r(doCounter)%i1) 			error stop 31011
	if (.not. precision_r4 (dvt1(doCounter)%r1,dvt1_r(doCounter)%r1)) 	error stop 31012
	if (dvt1(doCounter)%l1 		.NEQV. 	dvt1_r(doCounter)%l1) 			error stop 31013
	if (.not. precision_x8 (dvt1(doCounter)%c1,dvt1_r(doCounter)%c1)) 	error stop 31014
	if (dvt1(doCounter)%char1 	.ne. 	dvt1_r(doCounter)%char1) 		error stop 31015
end do
call sub32(dvt1,dvt1)
do doCounter=1,SIZEOFA
	if (dvt1(doCounter)%i1 		.ne. 	dvt1_r(doCounter)%i1) 			error stop 32011
	if (.not. precision_r4 (dvt1(doCounter)%r1,dvt1_r(doCounter)%r1)) 	error stop 32012
	if (dvt1(doCounter)%l1 		.NEQV. 	dvt1_r(doCounter)%l1) 			error stop 32013
	if (.not. precision_x8 (dvt1(doCounter)%c1,dvt1_r(doCounter)%c1)) 	error stop 32014
	if (dvt1(doCounter)%char1 	.ne. 	dvt1_r(doCounter)%char1) 		error stop 32015
end do

contains

subroutine sub11(arg,n)
    type(t1) :: arg(n)
	integer n
	value arg

	do doCounter=1,SIZEOFA
		if (arg(doCounter)%i1 		.ne. 	dvt1(doCounter)%i1) 		error stop 11101
		if (.not. precision_r4 (arg(doCounter)%r1,dvt1(doCounter)%r1)) 	error stop 11102
		if (arg(doCounter)%l1 		.NEQV. 	dvt1(doCounter)%l1) 		error stop 11103
		if (.not. precision_x8 (arg(doCounter)%c1,dvt1(doCounter)%c1)) 	error stop 11104
		if (arg(doCounter)%char1 	.ne. 	dvt1(doCounter)%char1) 		error stop 11105
	end do
	if (size(arg) .ne. SIZEOFA) error stop 1111
	if ( any(lbound(arg) .ne. 1)) error stop 1112
	if ( any(ubound(arg) .ne. SIZEOFA)) error stop 1113
	if (rank(arg) .ne. 1) error stop 1114
	if (any(shape(arg) .ne. SIZEOFA)) error stop 1115
	arg	= t1(400,4*atan(1.0),.false.,(3*atan(1.0),7*atan(1.0)),"6pq94jv382")
end subroutine

subroutine sub12(arg)
    type(t1) :: arg(10)
	value arg

	do doCounter=1,SIZEOFA
		if (arg(doCounter)%i1 		.ne. 	dvt1(doCounter)%i1) 		error stop 12101
		if (.not. precision_r4 (arg(doCounter)%r1,dvt1(doCounter)%r1)) 	error stop 12102
		if (arg(doCounter)%l1 		.NEQV. 	dvt1(doCounter)%l1) 		error stop 12103
		if (.not. precision_x8 (arg(doCounter)%c1,dvt1(doCounter)%c1)) 	error stop 12104
		if (arg(doCounter)%char1 	.ne. 	dvt1(doCounter)%char1) 		error stop 12105
	end do
	if (size(arg) .ne. SIZEOFA) error stop 1211
	if ( any(lbound(arg) .ne. 1)) error stop 1212
	if ( any(ubound(arg) .ne. SIZEOFA)) error stop 1213
	if (rank(arg) .ne. 1) error stop 1214
	if (any(shape(arg) .ne. SIZEOFA)) error stop 1215
	arg	= t1(400,4*atan(1.0),.false.,(3*atan(1.0),7*atan(1.0)),"6pq94jv382")
end subroutine

subroutine sub13(arg)
    type(t1) :: arg(SIZEOFA)
	value arg

	do doCounter=1,SIZEOFA
		if (arg(doCounter)%i1 		.ne. 	dvt1(doCounter)%i1) 		error stop 13101
		if (.not. precision_r4 (arg(doCounter)%r1,dvt1(doCounter)%r1)) 	error stop 13102
		if (arg(doCounter)%l1 		.NEQV. 	dvt1(doCounter)%l1) 		error stop 13103
		if (.not. precision_x8 (arg(doCounter)%c1,dvt1(doCounter)%c1)) 	error stop 13104
		if (arg(doCounter)%char1 	.ne. 	dvt1(doCounter)%char1) 		error stop 13105
	end do
	if (size(arg) .ne. SIZEOFA) error stop 1311
	if ( any(lbound(arg) .ne. 1)) error stop 1312
	if ( any(ubound(arg) .ne. SIZEOFA)) error stop 1313
	if (rank(arg) .ne. 1) error stop 1314
	if (any(shape(arg) .ne. SIZEOFA)) error stop 1315
	arg	= t1(400,4*atan(1.0),.false.,(3*atan(1.0),7*atan(1.0)),"6pq94jv382")
end subroutine

subroutine sub14(arg)
    type(t1) :: arg(:)
	value arg

	do doCounter=1,SIZEOFA
		if (arg(doCounter)%i1 		.ne. 	dvt1(doCounter)%i1) 		error stop 14101
		if (.not. precision_r4 (arg(doCounter)%r1,dvt1(doCounter)%r1)) 	error stop 14102
		if (arg(doCounter)%l1 		.NEQV. 	dvt1(doCounter)%l1) 		error stop 14103
		if (.not. precision_x8 (arg(doCounter)%c1,dvt1(doCounter)%c1)) 	error stop 14104
		if (arg(doCounter)%char1 	.ne. 	dvt1(doCounter)%char1) 		error stop 14105
	end do
	if (size(arg) .ne. SIZEOFA) error stop 1411
	if ( any(lbound(arg) .ne. 1)) error stop 1412
	if ( any(ubound(arg) .ne. SIZEOFA)) error stop 1413
	if (rank(arg) .ne. 1) error stop 1414
	if (any(shape(arg) .ne. SIZEOFA)) error stop 1415
	arg	= t1(400,4*atan(1.0),.false.,(3*atan(1.0),7*atan(1.0)),"6pq94jv382")
end subroutine

subroutine sub21(arg,n)
	integer n
	type(t1), DIMENSION(n) :: arg
	value arg

	do doCounter=1,SIZEOFA
		if (arg(doCounter)%i1 		.ne. 	dvt1(doCounter)%i1) 		error stop 21101
		if (.not. precision_r4 (arg(doCounter)%r1,dvt1(doCounter)%r1)) 	error stop 21102
		if (arg(doCounter)%l1 		.NEQV. 	dvt1(doCounter)%l1) 		error stop 21103
		if (.not. precision_x8 (arg(doCounter)%c1,dvt1(doCounter)%c1)) 	error stop 21104
		if (arg(doCounter)%char1 	.ne. 	dvt1(doCounter)%char1) 		error stop 21105
	end do
	if (size(arg) .ne. SIZEOFA) error stop 2111
	if ( any(lbound(arg) .ne. 1)) error stop 2112
	if ( any(ubound(arg) .ne. SIZEOFA)) error stop 2113
	if (rank(arg) .ne. 1) error stop 2114
	if (any(shape(arg) .ne. SIZEOFA)) error stop 2115
	arg	= t1(400,4*atan(1.0),.false.,(3*atan(1.0),7*atan(1.0)),"6pq94jv382")
end subroutine

subroutine sub22(arg)
	type(t1), DIMENSION(10) :: arg
	value arg

	do doCounter=1,SIZEOFA
		if (arg(doCounter)%i1 		.ne. 	dvt1(doCounter)%i1) 		error stop 22101
		if (.not. precision_r4 (arg(doCounter)%r1,dvt1(doCounter)%r1)) 	error stop 22102
		if (arg(doCounter)%l1 		.NEQV. 	dvt1(doCounter)%l1) 		error stop 22103
		if (.not. precision_x8 (arg(doCounter)%c1,dvt1(doCounter)%c1)) 	error stop 22104
		if (arg(doCounter)%char1 	.ne. 	dvt1(doCounter)%char1) 		error stop 22105
	end do
	if (size(arg) .ne. SIZEOFA) error stop 2211
	if ( any(lbound(arg) .ne. 1)) error stop 2212
	if ( any(ubound(arg) .ne. SIZEOFA)) error stop 2213
	if (rank(arg) .ne. 1) error stop 2214
	if (any(shape(arg) .ne. SIZEOFA)) error stop 2215
	arg	= t1(400,4*atan(1.0),.false.,(3*atan(1.0),7*atan(1.0)),"6pq94jv382")
end subroutine

subroutine sub23(arg)
	type(t1), DIMENSION(SIZEOFA) :: arg
	value arg

	do doCounter=1,SIZEOFA
		if (arg(doCounter)%i1 		.ne. 	dvt1(doCounter)%i1) 		error stop 23101
		if (.not. precision_r4 (arg(doCounter)%r1,dvt1(doCounter)%r1)) 	error stop 23102
		if (arg(doCounter)%l1 		.NEQV. 	dvt1(doCounter)%l1) 		error stop 23103
		if (.not. precision_x8 (arg(doCounter)%c1,dvt1(doCounter)%c1)) 	error stop 23104
		if (arg(doCounter)%char1 	.ne. 	dvt1(doCounter)%char1) 		error stop 23105
	end do
	if (size(arg) .ne. SIZEOFA) error stop 2311
	if ( any(lbound(arg) .ne. 1)) error stop 2312
	if ( any(ubound(arg) .ne. SIZEOFA)) error stop 2313
	if (rank(arg) .ne. 1) error stop 2314
	if (any(shape(arg) .ne. SIZEOFA)) error stop 2315
	arg	= t1(400,4*atan(1.0),.false.,(3*atan(1.0),7*atan(1.0)),"6pq94jv382")
end subroutine

subroutine sub24(arg)
	type(t1), DIMENSION(:) :: arg
	value arg

	do doCounter=1,SIZEOFA
		if (arg(doCounter)%i1 		.ne. 	dvt1(doCounter)%i1) 		error stop 24101
		if (.not. precision_r4 (arg(doCounter)%r1,dvt1(doCounter)%r1)) 	error stop 24102
		if (arg(doCounter)%l1 		.NEQV. 	dvt1(doCounter)%l1) 		error stop 24103
		if (.not. precision_x8 (arg(doCounter)%c1,dvt1(doCounter)%c1)) 	error stop 24104
		if (arg(doCounter)%char1 	.ne. 	dvt1(doCounter)%char1) 		error stop 24105
	end do
	if (size(arg) .ne. SIZEOFA) error stop 2411
	if ( any(lbound(arg) .ne. 1)) error stop 2412
	if ( any(ubound(arg) .ne. SIZEOFA)) error stop 2413
	if (rank(arg) .ne. 1) error stop 2414
	if (any(shape(arg) .ne. SIZEOFA)) error stop 2415
	arg	= t1(400,4*atan(1.0),.false.,(3*atan(1.0),7*atan(1.0)),"6pq94jv382")
end subroutine

subroutine sub31(arg,arg2)
	type(t1) arg2(:)
	type(t1) arg(size(arg2))
	value arg

	do doCounter=1,SIZEOFA
		if (arg(doCounter)%i1 		.ne. 	dvt1(doCounter)%i1) 		error stop 31101
		if (.not. precision_r4 (arg(doCounter)%r1,dvt1(doCounter)%r1)) 	error stop 31102
		if (arg(doCounter)%l1 		.NEQV. 	dvt1(doCounter)%l1) 		error stop 31103
		if (.not. precision_x8 (arg(doCounter)%c1,dvt1(doCounter)%c1)) 	error stop 31104
		if (arg(doCounter)%char1 	.ne. 	dvt1(doCounter)%char1) 		error stop 31105
	end do
	if (size(arg) .ne. SIZEOFA) error stop 3111
	if ( any(lbound(arg) .ne. 1)) error stop 3112
	if ( any(ubound(arg) .ne. SIZEOFA)) error stop 3113
	if (rank(arg) .ne. 1) error stop 3114
	if (any(shape(arg) .ne. SIZEOFA)) error stop 3115
	arg	= t1(400,4*atan(1.0),.false.,(3*atan(1.0),7*atan(1.0)),"6pq94jv382")
end subroutine

subroutine sub32(arg,arg2)
	type(t1) arg2(:)
	type(t1) , DIMENSION (size(arg2)) :: arg
	value arg

	do doCounter=1,SIZEOFA
		if (arg(doCounter)%i1 		.ne. 	dvt1(doCounter)%i1) 		error stop 32101
		if (.not. precision_r4 (arg(doCounter)%r1,dvt1(doCounter)%r1)) 	error stop 32102
		if (arg(doCounter)%l1 		.NEQV. 	dvt1(doCounter)%l1) 		error stop 32103
		if (.not. precision_x8 (arg(doCounter)%c1,dvt1(doCounter)%c1)) 	error stop 32104
		if (arg(doCounter)%char1 	.ne. 	dvt1(doCounter)%char1) 		error stop 32105
	end do
	if (size(arg) .ne. SIZEOFA) error stop 3211
	if ( any(lbound(arg) .ne. 1)) error stop 3212
	if ( any(ubound(arg) .ne. SIZEOFA)) error stop 3213
	if (rank(arg) .ne. 1) error stop 3214
	if (any(shape(arg) .ne. SIZEOFA)) error stop 3215
	arg	= t1(400,4*atan(1.0),.false.,(3*atan(1.0),7*atan(1.0)),"6pq94jv382")
end subroutine

end