!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : F2008/value/array/func/valuearray_f223.f
!*
!*  DATE                       : 2015-09-24
!*
!*  PRIMARY FUNCTIONS TESTED   : VALUE(F2008 extension) - dummy argument arrays allowed with value
!*
!*  DESCRIPTION                : testing the extensions to the VALUE attribute
!*                         		for passing non-contiguous array sections of derived types
!*									testing will check that
!*								1. dummy argument is equal to the actual argument
!*								2. actual argument doesn't change
!* =============================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901234567890

implicit none

type t1
  integer i1(6)
  integer i2
end type

integer SIZEOFA, doCounter, doCount2
parameter (SIZEOFA = 6)

type(t1) dvt1,dvt1_r
type(t1) dvt2(6),dvt2_r(6)

dvt1 	= t1((/1,2,3,4,5,6/),10)
dvt1_r 	= t1((/1,2,3,4,5,6/),10)

dvt2(1) 	= t1((/1,2,3,4,5,6/),10)
dvt2(2) 	= t1((/2,3,4,5,6,1/),20)
dvt2(3) 	= t1((/3,4,5,6,1,2/),30)
dvt2(4) 	= t1((/4,5,6,1,2,3/),40)
dvt2(5) 	= t1((/5,6,1,2,3,4/),50)
dvt2(6) 	= t1((/6,1,2,3,4,5/),60)
dvt2_r(1) 	= t1((/1,2,3,4,5,6/),10)
dvt2_r(2) 	= t1((/2,3,4,5,6,1/),20)
dvt2_r(3) 	= t1((/3,4,5,6,1,2/),30)
dvt2_r(4) 	= t1((/4,5,6,1,2,3/),40)
dvt2_r(5) 	= t1((/5,6,1,2,3,4/),50)
dvt2_r(6) 	= t1((/6,1,2,3,4,5/),60)

call sub1_int(dvt1%i1)
	do doCount2=1,6
		if (dvt1%i1(doCount2) .ne. dvt1_r%i1(doCount2)) error stop 11
	end do
	if (dvt1%i2 .ne. dvt1_r%i2) error stop 12

call sub2_int(dvt1%i1(2:5))
	do doCount2=1,6
		if (dvt1%i1(doCount2) .ne. dvt1_r%i1(doCount2)) error stop 21
	end do
	if (dvt1%i2 .ne. dvt1_r%i2) error stop 22

call sub3_int(dvt1%i1(2:5:2))
	do doCount2=1,6
		if (dvt1%i1(doCount2) .ne. dvt1_r%i1(doCount2)) error stop 31
	end do
	if (dvt1%i2 .ne. dvt1_r%i2) error stop 32

call sub4_int(dvt2%i2)
	do doCounter=1,6
		do doCount2=1,6
			if (dvt2(doCounter)%i1(doCount2) .ne. dvt2_r(doCounter)%i1(doCount2)) error stop 41
		end do
		if (dvt2(doCounter)%i2 .ne. dvt2_r(doCounter)%i2) error stop 42
	end do

call sub5_int(dvt2%i1(3))
	do doCounter=1,6
		do doCount2=1,6
			if (dvt2(doCounter)%i1(doCount2) .ne. dvt2_r(doCounter)%i1(doCount2)) error stop 51
		end do
		if (dvt2(doCounter)%i2 .ne. dvt2_r(doCounter)%i2) error stop 52
	end do

call sub6_int(dvt2(2:5)%i2)
	do doCounter=1,6
		do doCount2=1,6
			if (dvt2(doCounter)%i1(doCount2) .ne. dvt2_r(doCounter)%i1(doCount2)) error stop 61
		end do
		if (dvt2(doCounter)%i2 .ne. dvt2_r(doCounter)%i2) error stop 62
	end do

call sub7_int(dvt2(3:5)%i1(3))
	do doCounter=1,6
		do doCount2=1,6
			if (dvt2(doCounter)%i1(doCount2) .ne. dvt2_r(doCounter)%i1(doCount2)) error stop 71
		end do
		if (dvt2(doCounter)%i2 .ne. dvt2_r(doCounter)%i2) error stop 72
	end do

call sub8_int(dvt2(2:5:2)%i2)
	do doCounter=1,6
		do doCount2=1,6
			if (dvt2(doCounter)%i1(doCount2) .ne. dvt2_r(doCounter)%i1(doCount2)) error stop 81
		end do
		if (dvt2(doCounter)%i2 .ne. dvt2_r(doCounter)%i2) error stop 82
	end do

call sub9_int(dvt2(2:5:2)%i1(3))
	do doCounter=1,6
		do doCount2=1,6
			if (dvt2(doCounter)%i1(doCount2) .ne. dvt2_r(doCounter)%i1(doCount2)) error stop 91
		end do
		if (dvt2(doCounter)%i2 .ne. dvt2_r(doCounter)%i2) error stop 92
	end do

contains

subroutine sub1_int(arg) !(dvt1%i1) (/1,2,3,4,5,6/)
    integer*4, value :: arg(:)
	do doCounter=1,6
		if (arg(doCounter) .ne. doCounter) error stop 10
	end do

	do doCounter=1,size(arg)
		arg(doCounter) = 6+doCounter
	end do
end subroutine

subroutine sub2_int(arg) !dvt1%i1(2:5) (/2,3,4,5/)
    integer*4, value :: arg(:)
	do doCounter=1,4
		if (arg(doCounter) .ne. doCounter+1) error stop 20
	end do

	do doCounter=1,size(arg)
		arg(doCounter) = 6+doCounter
	end do
end subroutine

subroutine sub3_int(arg) !dvt1%i1(2:5:2) (/2,4/)
    integer*4, value :: arg(:)
	do doCounter=1,2
		if (arg(doCounter) .ne. doCounter*2) error stop 30
	end do

	do doCounter=1,size(arg)
		arg(doCounter) = 6+doCounter
	end do
end subroutine

subroutine sub4_int(arg) !dvt2%i2 (/10,20,30,40,50,60/)
    integer*4, value :: arg(:)
	do doCounter=1,6
		if (arg(doCounter) .ne. 10*doCounter) error stop 40
	end do

	do doCounter=1,size(arg)
		arg(doCounter) = 6+doCounter
	end do
end subroutine

subroutine sub5_int(arg) !dvt2%i1(3) (/3,4,5,6,1,2/)
    integer*4, value :: arg(:)
	do doCounter=1,6
		if (arg(doCounter) .ne. mod((doCounter+1),6)+1) error stop 50
	end do

	do doCounter=1,size(arg)
		arg(doCounter) = 6+doCounter
	end do
end subroutine

subroutine sub6_int(arg) !dvt2(2:5) (/20,30,40,50/)
    integer*4, value :: arg(:)
	do doCounter=1,4
		if (arg(doCounter) .ne. (doCounter+1)*10) error stop 60
	end do

	do doCounter=1,size(arg)
		arg(doCounter) = 6+doCounter
	end do
end subroutine

subroutine sub7_int(arg) !dvt2(3:5)%i1(3) (/5,6,1/)
    integer*4, value :: arg(:)
	do doCounter=1,3
		if (arg(doCounter) .ne. mod((doCounter+3),6)+1) error stop 70
	end do

	do doCounter=1,size(arg)
		arg(doCounter) = 6+doCounter
	end do
end subroutine

subroutine sub8_int(arg) !dvt2(2:5:2)%i2 (/20,40/)
    integer*4, value :: arg(:)
	do doCounter=1,2
		if (arg(doCounter) .ne. doCounter*20) error stop 80
	end do

	do doCounter=1,size(arg)
		arg(doCounter) = 6+doCounter
	end do
end subroutine

subroutine sub9_int(arg) !dvt2(2:5:2)%i1(3) (/4,6/)
    integer*4, value :: arg(:)
	do doCounter=1,2
		if (arg(doCounter) .ne. 2*(doCounter-1)+4) error stop 90
	end do

	do doCounter=1,size(arg)
		arg(doCounter) = 6+doCounter
	end do
end subroutine

end
