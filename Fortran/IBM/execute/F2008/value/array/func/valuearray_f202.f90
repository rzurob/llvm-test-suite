!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : F2008/value/array/func/valuearray_f202.f
!*
!*  DATE                       : 2015-09-24
!*
!*  PRIMARY FUNCTIONS TESTED   : VALUE(F2008 extension) - dummy argument arrays allowed with value
!*
!*  DESCRIPTION                : testing the extensions to the VALUE attribute
!*                                for passing an array of different types as actual argument
!*								- each array passed is of length 1000
!*								testing will check that
!*								1. dummy argument is equal to the actual argument
!*								2. actual argument doesn't change
!* =============================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901234567890

module m

type t1
  integer i1
  real r1
  logical l1
  complex c1
  character(10) char1
end type

integer SIZEOFA, doCounter
parameter (SIZEOFA = 1000)
logical, external :: precision_x8, precision_r4

contains

subroutine sub1_int(arg)
    integer*4 :: arg(:)
	value arg
	if (any (arg .ne. 100)) error stop 110
	arg = 200
end subroutine

subroutine sub1_r(arg)
    real :: arg(:)
	value arg
	do doCounter=1,SIZEOFA
		if (.not. precision_r4(arg(doCounter),atan(1.0))) error stop 111
	end do
	arg=4*atan(1.0)
end subroutine

subroutine sub1_com(arg)
    complex*8 :: arg(:)
	value arg
	do doCounter=1,SIZEOFA
		if (.not. precision_x8(arg(doCounter),(atan(1.0),2*atan(1.0)))) error stop 112
	end do
	arg=(5*atan(1.0),7*atan(1.0))
end subroutine

subroutine sub1_char(arg)
    character(SIZEOFA) :: arg(:)
	value arg
	if (any (arg .ne. "1234567890")) error stop 113
	arg = "abcdefghij"
end subroutine

subroutine sub1_lg(arg)
    logical :: arg(:)
	value arg
	if (any (arg .NEQV. .true.)) error stop 114
	arg = .false.
end subroutine

subroutine sub1_dvt(arg)
    type(t1) :: arg(:)
	value arg
	do doCounter=1,SIZEOFA
		if (arg(doCounter)%i1 		.ne. 	100) 								error stop 11501
		if (.not. precision_r4 (arg(doCounter)%r1,atan(1.0))) 					error stop 11502
		if (arg(doCounter)%l1 		.NEQV. 	.true.) 							error stop 11503
		if (.not. precision_x8 (arg(doCounter)%c1,(2*atan(1.0),3*atan(1.0)))) 	error stop 11504
		if (arg(doCounter)%char1 	.ne. 	"1a3b5c7d9e") 						error stop 11505
	end do
	arg	= t1(400,4*atan(1.0),.false.,(3*atan(1.0),7*atan(1.0)),"6pq94jv382")
end subroutine

end module m

program main
use m
implicit none


integer*4 i1(SIZEOFA),i1_r(SIZEOFA)
real  r1(SIZEOFA),r1_r(SIZEOFA)
complex*8  com1(SIZEOFA),com1_r(SIZEOFA)
character(SIZEOFA) c1(SIZEOFA),c1_r(SIZEOFA)
logical  l1(SIZEOFA),l1_r(SIZEOFA)
type(t1) dvt1(SIZEOFA),dvt1_r(SIZEOFA)

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
	if (.not. precision_r4(r1(doCounter),r1_r(doCounter))) error stop 11
	end do

call sub1_com(com1)
	do doCounter=1,SIZEOFA
	if (.not. precision_x8(com1(doCounter),com1_r(doCounter))) error stop 12
	end do

call sub1_char(c1)
if (any (c1 .ne. c1_r)) error stop 13

call sub1_lg(l1)
if (any (l1 .NEQV. l1_r)) error stop 14

call sub1_dvt(dvt1)
do doCounter=1,SIZEOFA
	if (dvt1(doCounter)%i1 		.ne. 	dvt1_r(doCounter)%i1) 			error stop 1501
	if (.not. precision_r4 (dvt1(doCounter)%r1,dvt1_r(doCounter)%r1)) 	error stop 1502
	if (dvt1(doCounter)%l1 		.NEQV. 	dvt1_r(doCounter)%l1) 			error stop 1503
	if (.not. precision_x8 (dvt1(doCounter)%c1,dvt1_r(doCounter)%c1)) 	error stop 1504
	if (dvt1(doCounter)%char1 	.ne. 	dvt1_r(doCounter)%char1) 		error stop 1505
end do

end
