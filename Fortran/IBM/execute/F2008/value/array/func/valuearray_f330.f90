!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : F2008/value/array/func/valuearray_f330.f
!*
!*  DATE                       : 2015-09-24
!*
!*  PRIMARY FUNCTIONS TESTED   : VALUE(F2008 extension) - dummy argument arrays allowed with value
!*
!*  DESCRIPTION                : testing the extensions to the VALUE attribute
!*             		for passing an array of different types to recursive subroutines
!*									testing will check that
!*								1. dummy argument is equal to the actual argument
!*								2. actual argument doesn't change
!* =============================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901234567890

implicit none

type t1
  integer i1
  real r1
  logical l1
end type

integer SIZEOFA, doCounter
parameter (SIZEOFA = 10)

logical, external :: precision_x8, precision_r4
integer*4 i1(10),i1_r(10)
real  r1(10),r1_r(10)
complex*8  com1(10),com1_r(10)
character(9) c1(10),c1_r(10)
logical  l1(10),l1_r(10)
type(t1) dvt1(10),dvt1_r(10)

i1  =500
i1_r=500

r1  =4*atan(1.0)
r1_r=4*atan(1.0)

com1  =(27*atan(1.0),9*atan(1.0))
com1_r=(27*atan(1.0),9*atan(1.0))

c1  = "123"
c1_r= "123"

l1  = .true.
l1_r= .true.

dvt1 	= t1(200,2*atan(1.0), .true.)
dvt1_r 	= t1(200,2*atan(1.0), .true.)


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
end do

contains

recursive subroutine sub1_int(arg)
    integer*4 :: arg(:)
	value arg
	if (any (arg==100)) then
		if (any (arg/=100)) error stop 1001
	else
		arg = arg - 100
		call sub1_int(arg)
	end if
end subroutine

recursive subroutine sub1_r(arg)
    real :: arg(:)
	value arg
	logical any100, any101
	real  tempReal
	tempReal=atan(1.0)

	any100=.false.
	any101=.false.

	do doCounter=1,SIZEOFA
		if (precision_r4(arg(doCounter),tempReal)) any100 = .true. ! one of the elements of arg is equal
	end do

	do doCounter=1,SIZEOFA
		if (.not. precision_r4(arg(doCounter),tempReal)) any101 = .true. ! one of the elements of arg is not equal
	end do

	if (any100) then
		if (any101) error stop 1101
	else
		arg = arg/2
		call sub1_r(arg)
	end if
end subroutine

recursive subroutine sub1_com(arg)
    complex*8 :: arg(:)
	value arg

	logical any100, any101
	complex*8 tempComp
	tempComp=(3*atan(1.0),atan(1.0))

	any100=.false.
	any101=.false.

	do doCounter=1,SIZEOFA
		if (precision_x8(arg(doCounter),tempComp)) any100 = .true. ! one of the elements of arg is equal
	end do

	do doCounter=1,SIZEOFA
		if (.not. precision_x8(arg(doCounter),tempComp)) any101 = .true. ! one of the elements of arg is not equal
	end do

	if (any100) then
		if (any101) error stop 1201
	else
		arg = arg/3
		call sub1_com(arg)
	end if

end subroutine

recursive subroutine sub1_char(arg)
    character(9) :: arg(:)
	value arg
	if (any (arg .eq. "123123123")) then
		if (any (arg .ne. "123123123")) error stop 1301
	else
		do doCounter=1,SIZEOFA
			arg(doCounter) = trim(arg(doCounter))//"123"
		end do
		call sub1_char(arg)
	end if

end subroutine

recursive subroutine sub1_lg(arg)
    logical :: arg(:)
	value arg
	if (any (arg .eqv. .false.)) then
		if (any (arg .neqv. .false.)) error stop 1401
	else
		arg = .not. arg
		call sub1_lg(arg)
	end if
end subroutine

recursive subroutine sub1_dvt(arg)
    type(t1) :: arg(:)
	value arg
	logical :: any100i,any100r,any100l = .false.
	logical :: any101i,any101r,any101l = .false.
	type(t1) tempDvt1
	tempDvt1 = t1(100,atan(1.0), .false.)

	any100i=.false.
	any101i=.false.
	any100r=.false.
	any101r=.false.
	any100l=.false.
	any101l=.false.

	do doCounter=1,SIZEOFA
		if (arg(doCounter)%i1 .eq. tempDvt1%i1) any100i = .true.
		if (precision_r4(arg(doCounter)%r1,tempDvt1%r1)) any100r = .true. ! one of the elements of arg is equal
		if (arg(doCounter)%l1 .EQV. tempDvt1%l1) any100l = .true.
	end do

	do doCounter=1,SIZEOFA
		if (arg(doCounter)%i1 .ne. tempDvt1%i1) any101i = .true.
		if (.not. precision_r4(arg(doCounter)%r1,tempDvt1%r1)) any101r = .true. ! one of the elements of arg is not equal
		if (arg(doCounter)%l1 .NEQV. tempDvt1%l1) any101l = .true.
	end do

	if (any100i .and. any100r .and. any100l) then
		if (any101i) error stop 1601
		if (any101r) error stop 1602
		if (any101l) error stop 1603
	else
		arg%i1 = arg%i1/2
		arg%r1 = arg%r1/2
		arg%l1 = .not. arg%l1
		call sub1_dvt(arg)
	end if

end subroutine

end
