!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : F2008/value/array/func/valuearray_f305b.f
!*
!*  DATE                       : 2015-09-24
!*
!*  PRIMARY FUNCTIONS TESTED   : VALUE(F2008 extension) - dummy argument arrays allowed with value
!*
!*  DESCRIPTION                : testing the extensions to the VALUE attribute
!*                         		for passing an array of different types to subroutines
!*								 contained in a module
!*									testing will check that
!*								1. dummy argument is equal to the actual argument
!*								2. actual argument doesn't change
!*								- breaking up module and main into two files
!* =============================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901234567890

program main
use m


integer*4 i1(10),i1_r(10)
real  r1(10),r1_r(10)
complex*8  com1(10),com1_r(10)
character(SIZEOFA) c1(10),c1_r(10)
logical  l1(10),l1_r(10)
type(t1) dvt1(10),dvt1_r(10)

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
