!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : F2008/value/array/func/valuearray_f303.f
!*
!*  PROGRAMMER                 : Cezar Lutac 
!*  DATE                       : 2015-09-24
!*
!*  PRIMARY FUNCTIONS TESTED   : VALUE(F2008 extension) - dummy argument arrays allowed with value
!*
!*  DESCRIPTION                : testing the extensions to the VALUE attribute
!*                         		for passing an array of different types to an 
!*								overloaded subroutine contained in a module	
!*									testing will check that
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

interface sub
	module procedure sub1_int,sub1_r,sub1_com,sub1_char,sub1_lg,sub1_dvt
end interface

integer SIZEOFA, doCounter
parameter (SIZEOFA = 10)
logical, external :: precision_x8, precision_r4

contains
  
subroutine sub1_int(arg)
    integer*4 :: arg(:)
	value arg
	
	if (any(arg .ne. 100)) error stop 1110
	if (size(arg) .ne. SIZEOFA) error stop 1111
	if ( any(lbound(arg) .ne. 1)) error stop 1112
	if ( any(ubound(arg) .ne. SIZEOFA)) error stop 1113
	if (rank(arg) .ne. 1) error stop 1114
	if (any(shape(arg) .ne. SIZEOFA)) error stop 1115

	call sub11_int(arg)
	if (any(arg .ne. 200)) error stop 1116
	call sub12_int(arg)
	if (any(arg .ne. 200)) error stop 1117
	
	contains
		subroutine sub11_int(arg)
			integer*4 :: arg(:)
			arg = 200
		end subroutine
		
		subroutine sub12_int(arg)
			integer*4 :: arg(:)
			value arg
			arg = 300
		end subroutine	
end subroutine

subroutine sub1_r(arg)
	real :: arg(:)
	value arg
	
	do doCounter=1,SIZEOFA
		if (.not. precision_r4(arg(doCounter), atan(1.0) )) error stop 1210
	end do
	if (size(arg) .ne. SIZEOFA) error stop 1211
	if ( any(lbound(arg) .ne. 1)) error stop 1212
	if ( any(ubound(arg) .ne. SIZEOFA)) error stop 1213
	if (rank(arg) .ne. 1) error stop 1214
	if (any(shape(arg) .ne. SIZEOFA)) error stop 1215

	call sub11_r(arg)
	do doCounter=1,SIZEOFA
		if (.not. precision_r4(arg(doCounter), 4*atan(1.0) )) error stop 1216
	end do	
	call sub12_r(arg)
	do doCounter=1,SIZEOFA
		if (.not. precision_r4(arg(doCounter), 4*atan(1.0) )) error stop 1217
	end do	
	
	contains
		subroutine sub11_r(arg)
			real :: arg(:)
			arg=4*atan(1.0)
		end subroutine
		
		subroutine sub12_r(arg)
			real :: arg(:)
			value arg
			arg=7*atan(1.0)
		end subroutine		
end subroutine

subroutine sub1_com(arg)
    complex*8 :: arg(:)
	value arg
	
	do doCounter=1,SIZEOFA
		if (.not. precision_x8(arg(doCounter),(atan(1.0),2*atan(1.0)))) error stop 1310
	end do
	if (size(arg) .ne. SIZEOFA) error stop 1311
	if ( any(lbound(arg) .ne. 1)) error stop 1312
	if ( any(ubound(arg) .ne. SIZEOFA)) error stop 1313
	if (rank(arg) .ne. 1) error stop 1314
	if (any(shape(arg) .ne. SIZEOFA)) error stop 1315

	call sub11_com(arg)
	do doCounter=1,SIZEOFA
		if (.not. precision_x8(arg(doCounter),(5*atan(1.0),7*atan(1.0)))) error stop 1316
	end do	
	call sub12_com(arg)
	do doCounter=1,SIZEOFA
		if (.not. precision_x8(arg(doCounter),(5*atan(1.0),7*atan(1.0)))) error stop 1317
	end do	
	
	contains
		subroutine sub11_com(arg)
			complex* 8:: arg(:)
			arg=(5*atan(1.0),7*atan(1.0))
		end subroutine
		
		subroutine sub12_com(arg)
			complex*8 :: arg(:)
			value arg
			arg=(11*atan(1.0),13*atan(1.0))
		end subroutine		
end subroutine	

subroutine sub1_char(arg)
    character(10) :: arg(:)
	value arg
	
	if (any (arg .ne. "1234567890")) error stop 1410
	if (size(arg) .ne. SIZEOFA) error stop 1411
	if ( any(lbound(arg) .ne. 1)) error stop 1412
	if ( any(ubound(arg) .ne. SIZEOFA)) error stop 1413
	if (rank(arg) .ne. 1) error stop 1414
	if (any(shape(arg) .ne. SIZEOFA)) error stop 1415
	
	call sub11_char(arg)
	if (any (arg .ne. "abcdefghij")) error stop 1416
	call sub12_char(arg)
	if (any (arg .ne. "abcdefghij")) error stop 1417
	
	contains
		subroutine sub11_char(arg)
			character(10) :: arg(:)
			arg = "abcdefghij"
		end subroutine
		
		subroutine sub12_char(arg)
			character(10) :: arg(:)
			value arg
			arg = "5a9v9a5jd8"
		end subroutine		
end subroutine

subroutine sub1_lg(arg)
    logical :: arg(:)
	value arg
	
	if (any (arg .NEQV. .true.)) error stop 1510
	if (size(arg) .ne. SIZEOFA) error stop 1511
	if ( any(lbound(arg) .ne. 1)) error stop 1512
	if ( any(ubound(arg) .ne. SIZEOFA)) error stop 1513
	if (rank(arg) .ne. 1) error stop 1514
	if (any(shape(arg) .ne. SIZEOFA)) error stop 1515

	call sub11_lg(arg)
	if (any (arg .NEQV. .false.)) error stop 1516
	call sub12_lg(arg)
	if (any (arg .NEQV. .false.)) error stop 1517
	
	contains
		subroutine sub11_lg(arg)
			logical :: arg(:)
			arg = .false.
		end subroutine
		
		subroutine sub12_lg(arg)
			logical :: arg(:)
			value arg
			arg = .true.
		end subroutine		
end subroutine	

subroutine sub1_dvt(arg)
    type(t1) :: arg(:)
	value arg

	do doCounter=1,SIZEOFA	  
		if (arg(doCounter)%i1 		.ne. 	100) 		error stop 16101
		if (.not. precision_r4 (arg(doCounter)%r1,atan(1.0))) 	error stop 16102
		if (arg(doCounter)%l1 		.NEQV. 	.true.) 		error stop 16103	
		if (.not. precision_x8 (arg(doCounter)%c1,(2*atan(1.0),3*atan(1.0)))) 	error stop 16104
		if (arg(doCounter)%char1 	.ne. 	"1a3b5c7d9e") 		error stop 16105
	end do	
	if (size(arg) .ne. SIZEOFA) error stop 1611
	if ( any(lbound(arg) .ne. 1)) error stop 1612
	if ( any(ubound(arg) .ne. SIZEOFA)) error stop 1613
	if (rank(arg) .ne. 1) error stop 1614
	if (any(shape(arg) .ne. SIZEOFA)) error stop 1615

	call sub11_dvt(arg)
	do doCounter=1,SIZEOFA	  
		if (arg(doCounter)%i1 		.ne. 	400) 								error stop 16161
		if (.not. precision_r4 (arg(doCounter)%r1,4*atan(1.0))) 				error stop 16162
		if (arg(doCounter)%l1 		.NEQV. 	.false.) 							error stop 16163	
		if (.not. precision_x8 (arg(doCounter)%c1,(3*atan(1.0),7*atan(1.0)))) 	error stop 16164
		if (arg(doCounter)%char1 	.ne. 	"6pq94jv382") 						error stop 16165
	end do	
	call sub12_dvt(arg)
	do doCounter=1,SIZEOFA	  
		if (arg(doCounter)%i1 		.ne. 	400) 								error stop 16171
		if (.not. precision_r4 (arg(doCounter)%r1,4*atan(1.0))) 				error stop 16172
		if (arg(doCounter)%l1 		.NEQV. 	.false.) 							error stop 16173	
		if (.not. precision_x8 (arg(doCounter)%c1,(3*atan(1.0),7*atan(1.0)))) 	error stop 16174
		if (arg(doCounter)%char1 	.ne. 	"6pq94jv382") 						error stop 16175
	end do	
	
	contains
		subroutine sub11_dvt(arg)
			type(t1) :: arg(:)
			arg	= t1(400,4*atan(1.0),.false.,(3*atan(1.0),7*atan(1.0)),"6pq94jv382")
		end subroutine
		
		subroutine sub12_dvt(arg)
			type(t1) :: arg(:)
			value arg
			arg	= t1(55,11*atan(1.0),.true.,(5*atan(1.0),9*atan(1.0)),"WA4tf7tooi")
		end subroutine		
end subroutine

end module m


program main
use m
implicit none

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


call sub(i1)
if (any (i1 .ne. i1_r)) error stop 10

call sub(r1)
	do doCounter=1,SIZEOFA
	if (.not. precision_r4(r1(doCounter),r1_r(doCounter))) error stop 11
	end do

call sub(com1)
	do doCounter=1,SIZEOFA
	if (.not. precision_x8(com1(doCounter),com1_r(doCounter))) error stop 12
	end do

call sub(c1)
if (any (c1 .ne. c1_r)) error stop 13

call sub(l1)
if (any (l1 .NEQV. l1_r)) error stop 14

call sub(dvt1)
do doCounter=1,SIZEOFA	  
	if (dvt1(doCounter)%i1 		.ne. 	dvt1_r(doCounter)%i1) 			error stop 1501
	if (.not. precision_r4 (dvt1(doCounter)%r1,dvt1_r(doCounter)%r1)) 	error stop 1502
	if (dvt1(doCounter)%l1 		.NEQV. 	dvt1_r(doCounter)%l1) 			error stop 1503	
	if (.not. precision_x8 (dvt1(doCounter)%c1,dvt1_r(doCounter)%c1)) 	error stop 1504
	if (dvt1(doCounter)%char1 	.ne. 	dvt1_r(doCounter)%char1) 		error stop 1505
end do	

end
