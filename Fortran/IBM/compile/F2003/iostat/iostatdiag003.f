!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan 9, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : is_iostat_eor is_iostat_end
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  : -qdebug=intmsg
!*
!*  DESCRIPTION                : Ensure that intrinsics can not be actual arguments

	implicit none

	integer :: i=-1
	logical :: result

	INTRINSIC is_iostat_end, is_iostat_eor

	!this one is ok
	result=func1(i,func2)

	!these two should issue a message
	result=func1(i,is_iostat_eor)
	result=func1(i,is_iostat_end)

	contains
	logical function func1(i,func)

	integer i
	interface
		logical function func(i)
		integer i
		end function func
	end interface
	func1=func(i)

	end function

	logical function func2(i)
	integer i
	if (i > 0) func2=.true.
	if (i < 0) func2=.false.
	if (i .eq. 0) func2=.false.
	end function
	end

