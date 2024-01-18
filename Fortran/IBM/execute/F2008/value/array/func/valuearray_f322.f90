!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : F2008/value/array/func/valuearray_f322.f
!*
!*  DATE                       : 2015-09-24
!*
!*  PRIMARY FUNCTIONS TESTED   : VALUE(F2008 extension) - dummy argument arrays allowed with value
!*
!*  DESCRIPTION                : testing the extensions to the VALUE attribute
!*                         		for passing an array of integers to subroutines
!*								containing optional dummy arguments
!*								- optional arguments are passed by value
!*						- non-optional arguments are passed without the value attribute
!*									testing will check that
!*								1. dummy argument is equal to the actual argument
!*								2. actual argument doesn't change
!* =============================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901234567890

implicit none

integer SIZEOFA, doCounter
parameter (SIZEOFA = 10)

integer*4 i1(10),i2(10),i3(10),i1_r(10)

i1  =500
i2  =100
i3 = 100
i1_r=100

!=================(arg1,op(arg2))======

call sub1_int1(i1,i2)
if (any (i1 .ne. i1_r)) error stop 1001
if (any (i2 .ne. i1_r)) error stop 1102
call sub1_int1(arg2=i2,arg1=i1)
if (any (i1 .ne. i1_r)) error stop 1002
if (any (i2 .ne. i1_r)) error stop 1102
call sub1_int1(i1)
if (any (i1 .ne. i1_r)) error stop 1003
if (any (i2 .ne. i1_r)) error stop 1103

!=================(op(arg1),arg2)======

call sub1_int2(i1,i2)
if (any (i1 .ne. i1_r)) error stop 2001
if (any (i2 .ne. i1_r)) error stop 2101
call sub1_int2(arg2=i2,arg1=i1)
if (any (i1 .ne. i1_r)) error stop 2002
if (any (i2 .ne. i1_r)) error stop 2102
call sub1_int2(arg2=i2)
if (any (i1 .ne. i1_r)) error stop 2003
if (any (i2 .ne. i1_r)) error stop 2103
!========(arg1,op(arg2),op(arg3))======

call sub1_int3(i1,i2,i3)
if (any (i1 .ne. i1_r)) error stop 3001
if (any (i2 .ne. i1_r)) error stop 3101
if (any (i3 .ne. i1_r)) error stop 3201
call sub1_int3(arg3=i3,arg2=i2,arg1=i1)
if (any (i1 .ne. i1_r)) error stop 3002
if (any (i2 .ne. i1_r)) error stop 3102
if (any (i3 .ne. i1_r)) error stop 3202
call sub1_int3(arg1=i1,arg3=i3,arg2=i2)
if (any (i1 .ne. i1_r)) error stop 3003
if (any (i2 .ne. i1_r)) error stop 3103
if (any (i3 .ne. i1_r)) error stop 3203
call sub1_int3(arg2=i2,arg1=i1,arg3=i3)
if (any (i1 .ne. i1_r)) error stop 3004
if (any (i2 .ne. i1_r)) error stop 3104
if (any (i3 .ne. i1_r)) error stop 3204
call sub1_int3(i1,arg3=i3,arg2=i2)
if (any (i1 .ne. i1_r)) error stop 3005
if (any (i2 .ne. i1_r)) error stop 3105
if (any (i3 .ne. i1_r)) error stop 3205
call sub1_int3(i1,i2,arg3=i1)
if (any (i1 .ne. i1_r)) error stop 3006
if (any (i2 .ne. i1_r)) error stop 3106
if (any (i3 .ne. i1_r)) error stop 3206
call sub1_int3(i1,i2)
if (any (i1 .ne. i1_r)) error stop 3007
if (any (i2 .ne. i1_r)) error stop 3107
if (any (i3 .ne. i1_r)) error stop 3207
call sub1_int3(i1,arg2=i2)
if (any (i1 .ne. i1_r)) error stop 3008
if (any (i2 .ne. i1_r)) error stop 3108
if (any (i3 .ne. i1_r)) error stop 3208
call sub1_int3(i1,arg3=i3)
if (any (i1 .ne. i1_r)) error stop 3009
if (any (i2 .ne. i1_r)) error stop 3109
if (any (i3 .ne. i1_r)) error stop 3209
call sub1_int3(i1)
if (any (i1 .ne. i1_r)) error stop 3010
if (any (i2 .ne. i1_r)) error stop 3110
if (any (i3 .ne. i1_r)) error stop 3210

!========(op(arg1),arg2,op(arg3))======

call sub1_int4(i1,i2,i3)
if (any (i1 .ne. i1_r)) error stop 4001
if (any (i2 .ne. i1_r)) error stop 4101
if (any (i3 .ne. i1_r)) error stop 4201
call sub1_int4(arg3=i3,arg2=i2,arg1=i1)
if (any (i1 .ne. i1_r)) error stop 4002
if (any (i2 .ne. i1_r)) error stop 4102
if (any (i3 .ne. i1_r)) error stop 4202
call sub1_int4(arg1=i1,arg3=i3,arg2=i2)
if (any (i1 .ne. i1_r)) error stop 4003
if (any (i2 .ne. i1_r)) error stop 4103
if (any (i3 .ne. i1_r)) error stop 4203
call sub1_int4(arg2=i2,arg1=i1,arg3=i3)
if (any (i1 .ne. i1_r)) error stop 4004
if (any (i2 .ne. i1_r)) error stop 4104
if (any (i3 .ne. i1_r)) error stop 4204
call sub1_int4(i1,arg3=i3,arg2=i2)
if (any (i1 .ne. i1_r)) error stop 4005
if (any (i2 .ne. i1_r)) error stop 4105
if (any (i3 .ne. i1_r)) error stop 4205
call sub1_int4(i1,i2,arg3=i3)
if (any (i1 .ne. i1_r)) error stop 4006
if (any (i2 .ne. i1_r)) error stop 4106
if (any (i3 .ne. i1_r)) error stop 4206
call sub1_int4(i1,i2)
if (any (i1 .ne. i1_r)) error stop 4007
if (any (i2 .ne. i1_r)) error stop 4107
if (any (i3 .ne. i1_r)) error stop 4207
call sub1_int4(arg2=i2,arg3=i3)
if (any (i1 .ne. i1_r)) error stop 4008
if (any (i2 .ne. i1_r)) error stop 4108
if (any (i3 .ne. i1_r)) error stop 4208
call sub1_int4(arg2=i2,arg1=i1)
if (any (i1 .ne. i1_r)) error stop 4009
if (any (i2 .ne. i1_r)) error stop 4109
if (any (i3 .ne. i1_r)) error stop 4209
call sub1_int4(arg2=i2)
if (any (i1 .ne. i1_r)) error stop 4010
if (any (i2 .ne. i1_r)) error stop 4110
if (any (i3 .ne. i1_r)) error stop 4210

contains

subroutine sub1_int1(arg1, arg2)
    integer*4 :: arg1(:)
	integer*4, optional :: arg2(:)
	value arg2
	arg1 = 100
	if (present(arg2)) arg2 = 300
end subroutine

subroutine sub1_int2(arg1, arg2)
    integer*4, optional :: arg1(:)
	integer*4 :: arg2(:)
	value arg1
	if (present(arg1)) arg1 = 200
	arg2 = 100
end subroutine

subroutine sub1_int3(arg1, arg2,arg3)
    integer*4 :: arg1(:)
	integer*4, optional :: arg2(:)
	integer*4, optional :: arg3(:)
	value arg2,arg3
	arg1 = 100
	if (present(arg2)) arg2 = 300
	if (present(arg3)) arg3 = 400
end subroutine

subroutine sub1_int4(arg1, arg2,arg3)
    integer*4, optional :: arg1(:)
	integer*4 :: arg2(:)
	integer*4, optional :: arg3(:)
	value arg1,arg3
	if (present(arg1)) arg1 = 200
	arg2 = 100
	if (present(arg3)) arg3 = 400
end subroutine

end
