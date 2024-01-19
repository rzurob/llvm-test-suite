! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : November 30, 2010
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Test that if F2008 langlvl is specified, the
!*				 compiler will issue error message when the
!*				 argument of POPPAR() is not on an integer type
!*
!*  TARGET(S)                  :
!* ===================================================================

program POPPAR_001d

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

	integer, dimension(POPPAR(5)) :: array
	integer, parameter :: j=POPPAR(5)

	print *, 'POPPAR(1)=				', POPPAR(i1)
	print *, 'POPPAR(10)=				', POPPAR(i2)
	print *, 'POPPAR(100)=				', POPPAR(i4)
	print *, 'POPPAR(1000)=				', POPPAR(i8)

	print *, 'POPPAR(5.0e0)=			', POPPAR(r4)
	print *, 'POPPAR(z8FF0000000000000)=', POPPAR(r8)

	print *, 'POPPAR(byte=10)=			', POPPAR(b)

	print *, 'POPPAR(.false.)=			', POPPAR(l1)
	print *, 'POPPAR(.false.)=			', POPPAR(l2)
	print *, 'POPPAR(.true.)=			', POPPAR(l4)
	print *, 'POPPAR(.true.)=			', POPPAR(l8)

	print *, 'POPPAR("a")=				', POPPAR(ch1)
	print *, 'POPPAR("ab")=				', POPPAR(ch2)
	print *, 'POPPAR("abcdefghij")=		', POPPAR(ch10)

end program POPPAR_001d
