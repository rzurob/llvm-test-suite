! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : November 15, 2010
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Test that if F2008 langlvl is specified, the
!*				 compiler will issue error message when the
!*				 argument of POPCNT() is not on an integer type
!*
!*  TARGET(S)                  :
!* ===================================================================

program POPCNT_001d

	implicit none

	integer(1), parameter :: i1=1
	integer(2), parameter :: i2=10
	integer(4), parameter :: i4=100
	integer(8), parameter :: i8=1000

	real(4), parameter :: r4=5.0e0
	real(8), parameter :: r8=z'8FF0000000000000'

	byte, parameter :: b=10

	logical(1), parameter :: l1=.false.
	logical(2), parameter :: l2=.false.
	logical(4), parameter :: l4=.true.
	logical(8), parameter :: l8=.true.

	character(1), parameter :: ch1='a'
	character(2), parameter :: ch2='ab'
	character(10), parameter :: ch10='abcdefghij'

	integer, dimension(popcnt(5)) :: array
	integer, parameter :: j=POPCNT(5)

	print *, 'POPCNT(1)=				', POPCNT(i1)
	print *, 'POPCNT(10)=				', POPCNT(i2)
	print *, 'POPCNT(100)=				', POPCNT(i4)
	print *, 'POPCNT(1000)=				', POPCNT(i8)

	print *, 'POPCNT(5.0e0)=			', POPCNT(r4)
	print *, 'POPCNT(z8FF0000000000000)=', POPCNT(r8)

	print *, 'POPCNT(byte=10)=			', POPCNT(b)

	print *, 'POPCNT(.false.)=			', POPCNT(l1)
	print *, 'POPCNT(.false.)=			', POPCNT(l2)
	print *, 'POPCNT(.true.)=			', POPCNT(l4)
	print *, 'POPCNT(.true.)=			', POPCNT(l8)

	print *, 'POPCNT("a")=				', POPCNT(ch1)
	print *, 'POPCNT("ab")=				', POPCNT(ch2)
	print *, 'POPCNT("abcdefghij")=		', POPCNT(ch10)

end program POPCNT_001d
