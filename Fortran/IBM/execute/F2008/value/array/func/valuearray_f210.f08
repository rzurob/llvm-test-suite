!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2015-09-24
!*
!*  PRIMARY FUNCTIONS TESTED   : VALUE(F2008 extension) - dummy argument arrays allowed with value
!*
!*  DESCRIPTION                : testing the extensions to the VALUE attribute
!*                         for passing an array constructor of different types as actual argument
!*								testing will check that
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

integer SIZEOFA, doCounter
parameter (SIZEOFA = 4)

logical, external :: precision_x8, precision_r4
integer*4 i1_r(4)
real  r1_r(4)
complex*8  com1_r(4)
character(10) c1_r(4)
logical  l1_r(4)
type(t1) dvt1_r(4)

i1_r=100
r1_r=atan(1.0)
com1_r=(atan(1.0),2*atan(1.0))
c1_r= "1234567890"
l1_r= .true.
dvt1_r 	= t1(100,atan(1.0)	, .true.,(2*atan(1.0),3*atan(1.0)),"1a3b5c7d9e")

call sub1_int((/ 100,100,100,100/))
call sub1_r ((/ atan(1.0),atan(1.0),atan(1.0),atan(1.0)/))
call sub1_com((/ (atan(1.0),2*atan(1.0)),(atan(1.0),2*atan(1.0)),(atan(1.0),2*atan(1.0)),(atan(1.0),2*atan(1.0))/))
call sub1_char((/ "1234567890","1234567890","1234567890","1234567890"/))
call sub1_lg((/ .true.,.true.,.true.,.true./))
call sub1_dvt((/ t1(100,atan(1.0)	, .true.,(2*atan(1.0),3*atan(1.0)),"1a3b5c7d9e"),t1(100,atan(1.0)	, .true.,(2*atan(1.0),3*atan(1.0)),"1a3b5c7d9e"),t1(100,atan(1.0)	, .true.,(2*atan(1.0),3*atan(1.0)),"1a3b5c7d9e"),t1(100,atan(1.0)	, .true.,(2*atan(1.0),3*atan(1.0)),"1a3b5c7d9e")/))

call sub11_int((/ 100,100,100,100/),4)
call sub12_int((/ 100,100,100,100/))
call sub13_int((/ 100,100,100,100/))
call sub21_int((/ 100,100,100,100/),4)
call sub22_int((/ 100,100,100,100/))
call sub23_int((/ 100,100,100,100/))
call sub24_int((/ 100,100,100,100/))
call sub31_int((/ 100,100,100,100/),(/ 100,100,100,100/))
call sub32_int((/ 100,100,100,100/),(/ 100,100,100,100/))

contains

subroutine sub1_int(arg)
    integer*4 :: arg(:)
	value arg
	if (any (arg .ne. i1_r)) 		error stop 10
	if (size(arg) .ne. 4) 			error stop 11
	if ( any(lbound(arg) .ne. 1)) 	error stop 12
	if ( any(ubound(arg) .ne. 4)) 	error stop 13
	if (rank(arg) .ne. 1) 			error stop 14
	if (any(shape(arg) .ne. 4)) 	error stop 15
end subroutine

subroutine sub1_r(arg)
    real :: arg(:)
	value arg
	do doCounter=1,SIZEOFA
		if (.not. precision_r4(arg(doCounter),r1_r(doCounter))) error stop 20
	end do
	if (size(arg) .ne. 4) 			error stop 21
	if ( any(lbound(arg) .ne. 1)) 	error stop 22
	if ( any(ubound(arg) .ne. 4)) 	error stop 23
	if (rank(arg) .ne. 1) 			error stop 24
	if (any(shape(arg) .ne. 4)) 	error stop 25
end subroutine

subroutine sub1_com(arg)
    complex*8 :: arg(:)
	value arg
	do doCounter=1,SIZEOFA
		if (.not. precision_x8(arg(doCounter),com1_r(doCounter))) error stop 30
	end do
	if (size(arg) .ne. 4) 			error stop 31
	if ( any(lbound(arg) .ne. 1)) 	error stop 32
	if ( any(ubound(arg) .ne. 4)) 	error stop 33
	if (rank(arg) .ne. 1) 			error stop 34
	if (any(shape(arg) .ne. 4)) 	error stop 35
end subroutine

subroutine sub1_char(arg)
    character(10) :: arg(:)
	value arg
	if (any (arg .ne. c1_r)) 		error stop 40
	if (size(arg) .ne. 4) 			error stop 41
	if ( any(lbound(arg) .ne. 1)) 	error stop 42
	if ( any(ubound(arg) .ne. 4)) 	error stop 43
	if (rank(arg) .ne. 1) 			error stop 44
	if (any(shape(arg) .ne. 4)) 	error stop 45
end subroutine

subroutine sub1_lg(arg)
    logical :: arg(:)
	value arg
	if (any (arg .NEQV. l1_r)) 		error stop 50
	if (size(arg) .ne. 4) 			error stop 51
	if ( any(lbound(arg) .ne. 1)) 	error stop 52
	if ( any(ubound(arg) .ne. 4)) 	error stop 53
	if (rank(arg) .ne. 1) 			error stop 54
	if (any(shape(arg) .ne. 4)) 	error stop 55
end subroutine

subroutine sub1_dvt(arg)
    type(t1) :: arg(:)
	value arg
	do doCounter=1,SIZEOFA
		if (arg(doCounter)%i1 		.ne. 	dvt1_r(doCounter)%i1) 			error stop 6001
		if (.not. precision_r4 (arg(doCounter)%r1,dvt1_r(doCounter)%r1)) 	error stop 6002
		if (arg(doCounter)%l1 		.NEQV. 	dvt1_r(doCounter)%l1) 			error stop 6003
		if (.not. precision_x8 (arg(doCounter)%c1,dvt1_r(doCounter)%c1)) 	error stop 6004
		if (arg(doCounter)%char1 	.ne. 	dvt1_r(doCounter)%char1) 		error stop 6005
	end do
	if (size(arg) .ne. 4) 			error stop 61
	if ( any(lbound(arg) .ne. 1)) 	error stop 62
	if ( any(ubound(arg) .ne. 4)) 	error stop 63
	if (rank(arg) .ne. 1) 			error stop 64
	if (any(shape(arg) .ne. 4)) 	error stop 65
end subroutine

! checking other invocations of dummy argument

subroutine sub11_int(arg,n)
    integer*4 :: arg(n), n
	value arg
	if (any (arg .ne. i1_r)) error stop 1
	if (size(arg) .ne. 4) 			error stop 111
	if ( any(lbound(arg) .ne. 1)) 	error stop 112
	if ( any(ubound(arg) .ne. 4)) 	error stop 113
	if (rank(arg) .ne. 1) 			error stop 114
	if (any(shape(arg) .ne. 4)) 	error stop 115
end subroutine

subroutine sub12_int(arg)
    integer*4 :: arg(4)
	value arg
	if (any (arg .ne. i1_r)) error stop 2
	if (size(arg) .ne. 4) 			error stop 121
	if ( any(lbound(arg) .ne. 1)) 	error stop 122
	if ( any(ubound(arg) .ne. 4)) 	error stop 123
	if (rank(arg) .ne. 1) 			error stop 124
	if (any(shape(arg) .ne. 4)) 	error stop 125
end subroutine

subroutine sub13_int(arg)
    integer*4 :: arg(SIZEOFA)
	value arg
	if (any (arg .ne. i1_r)) error stop 3
	if (size(arg) .ne. 4) 			error stop 131
	if ( any(lbound(arg) .ne. 1)) 	error stop 132
	if ( any(ubound(arg) .ne. 4)) 	error stop 133
	if (rank(arg) .ne. 1) 			error stop 134
	if (any(shape(arg) .ne. 4)) 	error stop 135
end subroutine

subroutine sub21_int(arg,n)
	integer n
	integer, DIMENSION(n) :: arg
	value arg
	if (any (arg .ne. i1_r)) error stop 4
	if (size(arg) .ne. 4) 			error stop 211
	if ( any(lbound(arg) .ne. 1)) 	error stop 212
	if ( any(ubound(arg) .ne. 4)) 	error stop 213
	if (rank(arg) .ne. 1) 			error stop 214
	if (any(shape(arg) .ne. 4)) 	error stop 215
end subroutine

subroutine sub22_int(arg)
	integer, DIMENSION(4) :: arg
	value arg
	if (any (arg .ne. i1_r)) error stop 5
	if (size(arg) .ne. 4) 			error stop 221
	if ( any(lbound(arg) .ne. 1)) 	error stop 222
	if ( any(ubound(arg) .ne. 4)) 	error stop 223
	if (rank(arg) .ne. 1) 			error stop 224
	if (any(shape(arg) .ne. 4)) 	error stop 225
end subroutine

subroutine sub23_int(arg)
	integer, DIMENSION(SIZEOFA) :: arg
	value arg
	if (any (arg .ne. i1_r)) error stop 6
	if (size(arg) .ne. 4) 			error stop 231
	if ( any(lbound(arg) .ne. 1)) 	error stop 232
	if ( any(ubound(arg) .ne. 4)) 	error stop 233
	if (rank(arg) .ne. 1) 			error stop 234
	if (any(shape(arg) .ne. 4)) 	error stop 235
end subroutine

subroutine sub24_int(arg)
	integer, DIMENSION(:) :: arg
	value arg
	if (any (arg .ne. i1_r)) error stop 7
	if (size(arg) .ne. 4) 			error stop 241
	if ( any(lbound(arg) .ne. 1)) 	error stop 242
	if ( any(ubound(arg) .ne. 4)) 	error stop 243
	if (rank(arg) .ne. 1) 			error stop 244
	if (any(shape(arg) .ne. 4)) 	error stop 245
end subroutine

subroutine sub31_int(arg,arg2)
	integer arg2(:)
	integer arg(size(arg2))
	value arg
	if (any (arg .ne. i1_r)) error stop 8
	if (size(arg) .ne. 4) 			error stop 311
	if ( any(lbound(arg) .ne. 1)) 	error stop 312
	if ( any(ubound(arg) .ne. 4)) 	error stop 313
	if (rank(arg) .ne. 1) 			error stop 314
	if (any(shape(arg) .ne. 4)) 	error stop 315
end subroutine

subroutine sub32_int(arg,arg2)
	integer arg2(:)
	integer , DIMENSION (size(arg2)) :: arg
	value arg
	if (any (arg .ne. i1_r)) error stop 9
	if (size(arg) .ne. 4) 			error stop 321
	if ( any(lbound(arg) .ne. 1)) 	error stop 322
	if ( any(ubound(arg) .ne. 4)) 	error stop 323
	if (rank(arg) .ne. 1) 			error stop 324
	if (any(shape(arg) .ne. 4)) 	error stop 325
end subroutine

end
