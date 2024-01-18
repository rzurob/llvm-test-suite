!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2015-09-24
!*
!*  PRIMARY FUNCTIONS TESTED   : VALUE(F2008 extension) - dummy argument arrays allowed with value
!*
!*  DESCRIPTION                : testing the extensions to the VALUE attribute
!*                   for arrays of different types of rank 2 with shape (10,10)
!*								- passing an array to a subroutine to check that
!*								1. dummy argument is equal to the actual argument
!*								2. actual argument doesn't change
!* =============================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901234567890

implicit none

type t1
  integer i1
  real r1
  logical l1
  complex c1
  character(10) char1
end type

integer SIZEOFA, doCounter,doCounter2
parameter (SIZEOFA = 10)

logical, external :: precision_x8, precision_r4
integer*4 i1(SIZEOFA,SIZEOFA),i1_r(SIZEOFA,SIZEOFA)
real  r1(SIZEOFA,SIZEOFA),r1_r(SIZEOFA,SIZEOFA)
complex*8  com1(SIZEOFA,SIZEOFA),com1_r(SIZEOFA,SIZEOFA)
character(SIZEOFA) c1(SIZEOFA,SIZEOFA),c1_r(SIZEOFA,SIZEOFA)
logical  l1(SIZEOFA,SIZEOFA),l1_r(SIZEOFA,SIZEOFA)
type(t1) dvt1(SIZEOFA,SIZEOFA),dvt1_r(SIZEOFA,SIZEOFA)

i1  =100
i1_r=100

r1  =atan(1.0)
r1_r=atan(1.0)

com1  =(atan(1.0),2*atan(1.0))
com1_r=(atan(1.0),2*atan(1.0))

c1  = "1234567890"
c1_r= "1234567890"

l1  = .true.
l1_r= .true.

dvt1 	= t1(100,atan(1.0)	, .true.,(2*atan(1.0),3*atan(1.0)),"1a3b5c7d9e")
dvt1_r 	= t1(100,atan(1.0)	, .true.,(2*atan(1.0),3*atan(1.0)),"1a3b5c7d9e")


call sub1_int(i1)
if (any (i1 .ne. i1_r)) error stop 10

call sub1_r(r1)
	do doCounter=1,SIZEOFA
		do doCounter2=1,SIZEOFA
			if (.not. precision_r4(r1(doCounter,doCounter2),r1_r(doCounter,doCounter2))) error stop 11
		end do
	end do

call sub1_com(com1)
	do doCounter=1,SIZEOFA
		do doCounter2=1,SIZEOFA
			if (.not. precision_x8(com1(doCounter,doCounter2),com1_r(doCounter,doCounter2))) error stop 12
		end do
	end do

call sub1_char(c1)
if (any (c1 .ne. c1_r)) error stop 13

call sub1_lg(l1)
if (any (l1 .NEQV. l1_r)) error stop 14

call sub1_dvt(dvt1)
	do doCounter=1,SIZEOFA
		do doCounter2=1,SIZEOFA
			if (dvt1(doCounter,doCounter2)%i1 		.ne. 	dvt1_r(doCounter,doCounter2)%i1) 			error stop 1501
			if (.not. precision_r4 (dvt1(doCounter,doCounter2)%r1,dvt1_r(doCounter,doCounter2)%r1)) 	error stop 1502
			if (dvt1(doCounter,doCounter2)%l1 		.NEQV. 	dvt1_r(doCounter,doCounter2)%l1) 			error stop 1503
			if (.not. precision_x8 (dvt1(doCounter,doCounter2)%c1,dvt1_r(doCounter,doCounter2)%c1)) 	error stop 1504
			if (dvt1(doCounter,doCounter2)%char1 	.ne. 	dvt1_r(doCounter,doCounter2)%char1) 		error stop 1505
		end do
	end do

contains

subroutine sub1_int(arg)
    integer*4 :: arg(:,:)
	value arg
	if (any (arg .ne. i1)) error stop 110

	!print*, "size", size(arg)
	!print*, "lbound", lbound(arg)
	!print*, "ubound", ubound(arg)
	!print*, "rank", rank(arg)
	!print*, "shape", shape(arg)
	!
	!print*, "=============="
	!
	!print*, "size", SIZEOFA**2
	!print*, "lbound", 1
	!print*, "ubound", SIZEOFA
	!print*, "rank", 2
	!print*, "shape", shape(arg)

	if (size(arg) .ne. SIZEOFA**2) 		error stop 101
	if ( any(lbound(arg) .ne. 1)) 		error stop 102
	if ( any(ubound(arg) .ne. SIZEOFA)) error stop 103
	if (rank(arg) .ne. 2) 				error stop 104
	if (any(shape(arg) .ne. SIZEOFA)) 	error stop 105
	arg = 200
end subroutine

subroutine sub1_r(arg)
    real :: arg(:,:)
	value arg
	do doCounter=1,SIZEOFA
		do doCounter2=1,SIZEOFA
			if (.not. precision_r4(arg(doCounter,doCounter2),r1(doCounter,doCounter2))) error stop 111
		end do
	end do
	if (size(arg) .ne. SIZEOFA**2) 		error stop 201
	if ( any(lbound(arg) .ne. 1)) 		error stop 202
	if ( any(ubound(arg) .ne. SIZEOFA)) error stop 203
	if (rank(arg) .ne. 2) 				error stop 204
	if (any(shape(arg) .ne. SIZEOFA)) 	error stop 205
	arg=4*atan(1.0)
end subroutine

subroutine sub1_com(arg)
    complex*8 :: arg(:,:)
	value arg
	do doCounter=1,SIZEOFA
		do doCounter2=1,SIZEOFA
			if (.not. precision_x8(arg(doCounter,doCounter2),com1(doCounter,doCounter2))) error stop 112
		end do
	end do
	if (size(arg) .ne. SIZEOFA**2) 		error stop 301
	if ( any(lbound(arg) .ne. 1)) 		error stop 302
	if ( any(ubound(arg) .ne. SIZEOFA)) error stop 303
	if (rank(arg) .ne. 2) 				error stop 304
	if (any(shape(arg) .ne. SIZEOFA)) 	error stop 305
	arg=(5*atan(1.0),7*atan(1.0))
end subroutine

subroutine sub1_char(arg)
    character(SIZEOFA) :: arg(:,:)
	value arg
	if (any (arg .ne. c1)) error stop 113
	if (size(arg) .ne. SIZEOFA**2) 		error stop 401
	if ( any(lbound(arg) .ne. 1)) 		error stop 402
	if ( any(ubound(arg) .ne. SIZEOFA)) error stop 403
	if (rank(arg) .ne. 2) 				error stop 404
	if (any(shape(arg) .ne. SIZEOFA)) 	error stop 405
	arg = "abcdefghij"
end subroutine

subroutine sub1_lg(arg)
    logical :: arg(:,:)
	value arg
	if (any (arg .NEQV. l1)) error stop 114
	if (size(arg) .ne. SIZEOFA**2) 		error stop 501
	if ( any(lbound(arg) .ne. 1)) 		error stop 502
	if ( any(ubound(arg) .ne. SIZEOFA)) error stop 503
	if (rank(arg) .ne. 2) 				error stop 504
	if (any(shape(arg) .ne. SIZEOFA)) 	error stop 505
	arg = .false.
end subroutine

subroutine sub1_dvt(arg)
    type(t1) :: arg(:,:)
	value arg
	do doCounter=1,SIZEOFA
		do doCounter2=1,SIZEOFA
			if (arg(doCounter,doCounter2)%i1 		.ne. 	dvt1(doCounter,doCounter2)%i1) 			error stop 11501
			if (.not. precision_r4 (arg(doCounter,doCounter2)%r1,dvt1(doCounter,doCounter2)%r1)) 	error stop 11502
			if (arg(doCounter,doCounter2)%l1 		.NEQV. 	dvt1(doCounter,doCounter2)%l1) 			error stop 11503
			if (.not. precision_x8 (arg(doCounter,doCounter2)%c1,dvt1(doCounter,doCounter2)%c1)) 	error stop 11504
			if (arg(doCounter,doCounter2)%char1 	.ne. 	dvt1(doCounter,doCounter2)%char1) 		error stop 11505
		end do
	end do
	if (size(arg) .ne. SIZEOFA**2) 		error stop 601
	if ( any(lbound(arg) .ne. 1)) 		error stop 602
	if ( any(ubound(arg) .ne. SIZEOFA)) error stop 603
	if (rank(arg) .ne. 2) 				error stop 604
	if (any(shape(arg) .ne. SIZEOFA)) 	error stop 605
	arg	= t1(400,4*atan(1.0),.false.,(3*atan(1.0),7*atan(1.0)),"6pq94jv382")
end subroutine

end
