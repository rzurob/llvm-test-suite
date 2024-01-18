!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : F2008/value/array/func/valuearray_f320.f
!*
!*  DATE                       : 2015-09-24
!*
!*  PRIMARY FUNCTIONS TESTED   : VALUE(F2008 extension) - dummy argument arrays allowed with value
!*
!*  DESCRIPTION                : testing the extensions to the VALUE attribute
!*                         		for passing an array of integers to subroutines
!*								containing optional dummy arguments
!*									testing will check that
!*								1. dummy argument is equal to the actual argument
!*								2. actual argument doesn't change
!* =============================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901234567890

implicit none

integer SIZEOFA, doCounter
parameter (SIZEOFA = 10)

integer*4 i1(10),i1_r(10)

i1  =100
i1_r=100

!=================(arg1,op(arg2))======

call sub1_int1(i1,i1)
if (any (i1 .ne. i1_r)) error stop 1001
call sub1_int1(arg2=i1,arg1=i1)
if (any (i1 .ne. i1_r)) error stop 1002
call sub1_int1(i1)
if (any (i1 .ne. i1_r)) error stop 1003

!=================(op(arg1),arg2)======

call sub1_int2(i1,i1)
if (any (i1 .ne. i1_r)) error stop 2001
call sub1_int2(arg2=i1,arg1=i1)
if (any (i1 .ne. i1_r)) error stop 2002
call sub1_int2(arg2=i1)
if (any (i1 .ne. i1_r)) error stop 2003

!========(arg1,op(arg2),op(arg3))======

call sub1_int3(i1,i1,i1)
if (any (i1 .ne. i1_r)) error stop 3001
call sub1_int3(arg3=i1,arg2=i1,arg1=i1)
if (any (i1 .ne. i1_r)) error stop 3002
call sub1_int3(arg1=i1,arg3=i1,arg2=i1)
if (any (i1 .ne. i1_r)) error stop 3003
call sub1_int3(arg2=i1,arg1=i1,arg3=i1)
if (any (i1 .ne. i1_r)) error stop 3004
call sub1_int3(i1,arg3=i1,arg2=i1)
if (any (i1 .ne. i1_r)) error stop 3005
call sub1_int3(i1,i1,arg3=i1)
if (any (i1 .ne. i1_r)) error stop 3006
call sub1_int3(i1,i1)
if (any (i1 .ne. i1_r)) error stop 3007
call sub1_int3(i1,arg2=i1)
if (any (i1 .ne. i1_r)) error stop 3008
call sub1_int3(i1,arg3=i1)
if (any (i1 .ne. i1_r)) error stop 3009
call sub1_int3(i1)
if (any (i1 .ne. i1_r)) error stop 3010

!========(op(arg1),arg2,op(arg3))======

call sub1_int4(i1,i1,i1)
if (any (i1 .ne. i1_r)) error stop 4001
call sub1_int4(arg3=i1,arg2=i1,arg1=i1)
if (any (i1 .ne. i1_r)) error stop 4002
call sub1_int4(arg1=i1,arg3=i1,arg2=i1)
if (any (i1 .ne. i1_r)) error stop 4003
call sub1_int4(arg2=i1,arg1=i1,arg3=i1)
if (any (i1 .ne. i1_r)) error stop 4004
call sub1_int4(i1,arg3=i1,arg2=i1)
if (any (i1 .ne. i1_r)) error stop 4005
call sub1_int4(i1,i1,arg3=i1)
if (any (i1 .ne. i1_r)) error stop 4006
call sub1_int4(i1,i1)
if (any (i1 .ne. i1_r)) error stop 4007
call sub1_int4(arg2=i1,arg3=i1)
if (any (i1 .ne. i1_r)) error stop 4008
call sub1_int4(arg2=i1,arg1=i1)
if (any (i1 .ne. i1_r)) error stop 4009
call sub1_int4(arg2=i1)
if (any (i1 .ne. i1_r)) error stop 4010

contains

subroutine sub1_int1(arg1, arg2)
    integer*4 :: arg1(:)
	integer*4, optional :: arg2(:)
	value arg1,arg2
	arg1 = 200
	if (present(arg2)) arg2 = 300
end subroutine

subroutine sub1_int2(arg1, arg2)
    integer*4, optional :: arg1(:)
	integer*4 :: arg2(:)
	value arg1,arg2
	if (present(arg1)) arg1 = 200
	arg2 = 300
end subroutine

subroutine sub1_int3(arg1, arg2,arg3)
    integer*4 :: arg1(:)
	integer*4, optional :: arg2(:)
	integer*4, optional :: arg3(:)
	value arg1,arg2,arg3
	arg1 = 200
	if (present(arg2)) arg2 = 300
	if (present(arg3)) arg3 = 400
end subroutine

subroutine sub1_int4(arg1, arg2,arg3)
    integer*4, optional :: arg1(:)
	integer*4 :: arg2(:)
	integer*4, optional :: arg3(:)
	value arg1,arg2,arg3
	if (present(arg1)) arg1 = 200
	arg2 = 300
	if (present(arg3)) arg3 = 400
end subroutine

end
