!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2015-09-24
!*
!*  PRIMARY FUNCTIONS TESTED   : VALUE(F2008 extension) - dummy argument arrays allowed with value
!*
!*  DESCRIPTION                : testing the extensions to the VALUE attribute
!*                   			for passing arrays of different types functions
!*								contained in a module
!*								- passing an array to a function to check that
!*								1. dummy argument is equal to the actual argument
!*								2. actual argument doesn't change
!*								3. function return is as expected
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
	logical, external :: precision_x8, precision_r4
	parameter (SIZEOFA = 10)

contains

integer*4 function func1_int(arg)
    integer*4 :: arg(:)
	value arg
	if (any (arg .ne. 200)) error stop 110
	arg = 100
	func1_int = 100
end

real function func1_r(arg)
    real :: arg(:)
	value arg
	do doCounter=1,SIZEOFA
		if (.not. precision_r4(arg(doCounter),2*atan(1.0))) error stop 111
	end do
	arg =atan(1.0)
	func1_r=atan(1.0)
end

complex*8 function func1_com(arg)
    complex*8 :: arg(:)
	value arg
	do doCounter=1,SIZEOFA
		if (.not. precision_x8(arg(doCounter),(2*atan(1.0),3*atan(1.0)))) error stop 112
	end do
	arg =(atan(1.0),2*atan(1.0))
	func1_com=(atan(1.0),2*atan(1.0))
end

character(SIZEOFA) function func1_char(arg)
    character(SIZEOFA) :: arg(:)
	value arg
	if (any (arg .ne. "9v8f3hz38f")) error stop 113
	arg = "1234567890"
	func1_char = "1234567890"
end

logical function func1_lg(arg)
    logical :: arg(:)
	value arg
	if (any (arg .NEQV. .false.)) error stop 114
	arg = .true.
	func1_lg = .true.
end

type(t1) function func1_dvt(arg)
    type(t1) :: arg(:)
	value arg
	do doCounter=1,SIZEOFA
		if (arg(doCounter)%i1 		.ne. 	200) 		error stop 11501
		if (.not. precision_r4 (arg(doCounter)%r1,2*atan(1.0))) 	error stop 11502
		if (arg(doCounter)%l1 		.NEQV. 	.false.) 		error stop 11503
		if (.not. precision_x8 (arg(doCounter)%c1,(3*atan(1.0),5*atan(1.0)))) 	error stop 11504
		if (arg(doCounter)%char1 	.ne. 	"z8ck29alv3") 		error stop 11505
	end do
	arg	= t1(100,atan(1.0)	, .true.,(2*atan(1.0),3*atan(1.0)),"1a3b5c7d9e")
	func1_dvt= t1(100,atan(1.0)	, .true.,(2*atan(1.0),3*atan(1.0)),"1a3b5c7d9e")
end

end module m

program main
use m

implicit none

integer*4 i1(10),i1_r(10),i1_f(10)
real  r1(10),r1_r(10),r1_f(10)
complex*8  com1(10),com1_r(10),com1_f(10)
character(SIZEOFA) c1(10),c1_r(10),c1_f(10)
logical  l1(10),l1_r(10),l1_f(10)
type(t1) dvt1(10),dvt1_r(10),dvt1_f(10)

i1  =200
i1_r=100

r1  =2*atan(1.0)
r1_r=atan(1.0)

com1  =(2*atan(1.0),3*atan(1.0))
com1_r=(atan(1.0),2*atan(1.0))

c1  = "9v8f3hz38f"
c1_r= "1234567890"

l1  = .false.
l1_r= .true.

dvt1 	= t1(200,2*atan(1.0), .false.,(3*atan(1.0),5*atan(1.0))	,"z8ck29alv3")
dvt1_r 	= t1(100,atan(1.0)	, .true.,(2*atan(1.0),3*atan(1.0))	,"1a3b5c7d9e")


i1_f = func1_int(i1)
if (any (i1_f .ne. i1_r)) error stop 10
if (any (i1 .eq. i1_r)) error stop 210

r1_f = func1_r(r1)
	do doCounter=1,SIZEOFA
		if (.not. precision_r4(r1_f(doCounter),r1_r(doCounter))) error stop 11
		if (precision_r4(r1(doCounter),r1_r(doCounter))) error stop 211
	end do

com1_f = func1_com(com1)
	do doCounter=1,SIZEOFA
		if (.not. precision_x8(com1_f(doCounter),com1_r(doCounter))) error stop 12
		if (precision_x8(com1(doCounter),com1_r(doCounter))) error stop 212
	end do

c1_f = func1_char(c1)
if (any (c1_f .ne. c1_r)) error stop 13
if (any (c1 .eq. c1_r)) error stop 213

l1_f=  func1_lg(l1)
if (any (l1_f .NEQV. l1_r)) error stop 14
if (any (l1 .EQV. l1_r)) error stop 214

dvt1_f = func1_dvt(dvt1)
do doCounter=1,SIZEOFA
	if (dvt1_f(doCounter)%i1 		.ne. 	dvt1_r(doCounter)%i1) 			error stop 1501
	if (.not. precision_r4 (dvt1_f(doCounter)%r1,dvt1_r(doCounter)%r1)) 	error stop 1502
	if (dvt1_f(doCounter)%l1 		.NEQV. 	dvt1_r(doCounter)%l1) 			error stop 1503
	if (.not. precision_x8 (dvt1_f(doCounter)%c1,dvt1_r(doCounter)%c1)) 	error stop 1504
	if (dvt1_f(doCounter)%char1 	.ne. 	dvt1_r(doCounter)%char1) 		error stop 1505

	if (dvt1(doCounter)%i1 		.eq. 	dvt1_r(doCounter)%i1) 		error stop 2501
	if (precision_r4 (dvt1(doCounter)%r1,dvt1_r(doCounter)%r1)) 	error stop 2502
	if (dvt1(doCounter)%l1 		.EQV. 	dvt1_r(doCounter)%l1) 		error stop 2503
	if (precision_x8 (dvt1(doCounter)%c1,dvt1_r(doCounter)%c1)) 	error stop 2504
	if (dvt1(doCounter)%char1 	.eq. 	dvt1_r(doCounter)%char1) 	error stop 2505
end do

end
