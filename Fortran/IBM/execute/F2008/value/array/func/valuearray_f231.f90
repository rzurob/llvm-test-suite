!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2015-09-24
!*
!*  PRIMARY FUNCTIONS TESTED   : VALUE(F2008 extension) - dummy argument arrays allowed with value
!*
!*  DESCRIPTION                : testing the extensions to the VALUE attribute
!*                         for passing an array of different types as actual argument
!*								testing will check that
!*								1. dummy argument is equal to the actual argument
!*								2. actual argument doesn't change
!*								- dummy array is defined as an adjustable array
!* =============================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901234567890

implicit none

type t1
  integer i1
  real r1
  logical l1
  complex c1
  character(9) char1
end type

integer SIZEOFA, doCounter
parameter (SIZEOFA = 9)

logical, external :: precision_x8, precision_r4
integer*4 i1(9),i1_r(9)
real  r1(9),r1_r(9)
complex*8  com1(9),com1_r(9)
character(SIZEOFA) c1(9),c1_r(9)
logical  l1(9),l1_r(9)
type(t1) dvt1(9),dvt1_r(9)

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


call sub1_int(i1,3)
if (any (i1 .ne. i1_r)) error stop 10

call sub1_r(r1,3)
	do doCounter=1,SIZEOFA
	if (.not. precision_r4(r1(doCounter),r1_r(doCounter))) error stop 11
	end do

call sub1_com(com1,3)
	do doCounter=1,SIZEOFA
	if (.not. precision_x8(com1(doCounter),com1_r(doCounter))) error stop 12
	end do

call sub1_char(c1,3)
if (any (c1 .ne. c1_r)) error stop 13

call sub1_lg(l1,3)
if (any (l1 .NEQV. l1_r)) error stop 14

call sub1_dvt(dvt1,3)
do doCounter=1,SIZEOFA
	if (dvt1(doCounter)%i1 		.ne. 	dvt1_r(doCounter)%i1) 			error stop 151
	if (.not. precision_r4 (dvt1(doCounter)%r1,dvt1_r(doCounter)%r1)) 	error stop 152
	if (dvt1(doCounter)%l1 		.NEQV. 	dvt1_r(doCounter)%l1) 			error stop 153
	if (.not. precision_x8 (dvt1(doCounter)%c1,dvt1_r(doCounter)%c1)) 	error stop 154
	if (dvt1(doCounter)%char1 	.ne. 	dvt1_r(doCounter)%char1) 		error stop 155
end do

contains

subroutine sub1_int(arg,arg2)
	integer arg2
    integer*4 :: arg(3*arg2)
	value arg
	if (any (arg .ne. i1)) error stop 110

	if (size(arg) .ne. SIZEOFA) error stop 111
	if ( any(lbound(arg) .ne. 1)) error stop 112
	if ( any(ubound(arg) .ne. SIZEOFA)) error stop 113
	if (rank(arg) .ne. 1) error stop 114
	if (any(shape(arg) .ne. SIZEOFA)) error stop 115
	arg = 200
end subroutine

subroutine sub1_r(arg,arg2)
	integer arg2
    real :: arg(3*arg2)
	value arg
	do doCounter=1,size(arg)
		if (.not. precision_r4(arg(doCounter),r1(doCounter))) error stop 210
	end do

	if (size(arg) .ne. SIZEOFA) error stop 211
	if ( any(lbound(arg) .ne. 1)) error stop 212
	if ( any(ubound(arg) .ne. SIZEOFA)) error stop 213
	if (rank(arg) .ne. 1) error stop 214
	if (any(shape(arg) .ne. SIZEOFA)) error stop 215
	arg=4*atan(1.0)
end subroutine

subroutine sub1_com(arg,arg2)
	integer arg2
    complex*8 :: arg(3*arg2)
	value arg
	do doCounter=1,size(arg)
		if (.not. precision_x8(arg(doCounter),com1(doCounter))) error stop 310
	end do

	if (size(arg) .ne. SIZEOFA) error stop 311
	if ( any(lbound(arg) .ne. 1)) error stop 312
	if ( any(ubound(arg) .ne. SIZEOFA)) error stop 313
	if (rank(arg) .ne. 1) error stop 314
	if (any(shape(arg) .ne. SIZEOFA)) error stop 315
	arg=(5*atan(1.0),7*atan(1.0))
end subroutine

subroutine sub1_char(arg,arg2)
	integer arg2
    character(SIZEOFA) :: arg(3*arg2)
	value arg
	if (any (arg .ne. c1)) error stop 410

	if (size(arg) .ne. SIZEOFA) error stop 411
	if ( any(lbound(arg) .ne. 1)) error stop 412
	if ( any(ubound(arg) .ne. SIZEOFA)) error stop 413
	if (rank(arg) .ne. 1) error stop 414
	if (any(shape(arg) .ne. SIZEOFA)) error stop 415
	arg = "abcdefghij"
end subroutine

subroutine sub1_lg(arg,arg2)
	integer arg2
    logical :: arg(3*arg2)
	value arg
	if (any (arg .NEQV. l1)) error stop 510

	if (size(arg) .ne. SIZEOFA) error stop 511
	if ( any(lbound(arg) .ne. 1)) error stop 512
	if ( any(ubound(arg) .ne. SIZEOFA)) error stop 513
	if (rank(arg) .ne. 1) error stop 514
	if (any(shape(arg) .ne. SIZEOFA)) error stop 515
	arg = .false.
end subroutine

subroutine sub1_dvt(arg,arg2)
	integer arg2
    type(t1) :: arg(3*arg2)
	value arg
	do doCounter=1,size(arg)
		if (arg(doCounter)%i1 		.ne. 	dvt1(doCounter)%i1) 		error stop 601
		if (.not. precision_r4 (arg(doCounter)%r1,dvt1(doCounter)%r1)) 	error stop 602
		if (arg(doCounter)%l1 		.NEQV. 	dvt1(doCounter)%l1) 		error stop 603
		if (.not. precision_x8 (arg(doCounter)%c1,dvt1(doCounter)%c1)) 	error stop 604
		if (arg(doCounter)%char1 	.ne. 	dvt1(doCounter)%char1) 		error stop 605
	end do

	if (size(arg) .ne. SIZEOFA) error stop 611
	if ( any(lbound(arg) .ne. 1)) error stop 612
	if ( any(ubound(arg) .ne. SIZEOFA)) error stop 613
	if (rank(arg) .ne. 1) error stop 614
	if (any(shape(arg) .ne. SIZEOFA)) error stop 615
	arg	= t1(400,4*atan(1.0),.false.,(3*atan(1.0),7*atan(1.0)),"6pq94jv382")
end subroutine

end
