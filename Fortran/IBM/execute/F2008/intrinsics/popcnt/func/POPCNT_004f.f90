! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : November 30, 2010
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Test that POPCNT can be used as
!*				 constant expression in an
!*				 initialization expressioan (in subroutine)
!*
!*  TARGET(S)                  :
!* ===================================================================
program POPCNT_004f

	implicit none

	interface
    subroutine SumPOPCNT_external(i,j, resA)
     integer, INTENT(IN) :: i,j
     integer, INTENT(OUT) :: resA
    end subroutine SumPOPCNT_external
    end interface

	integer :: res1, res1a
	integer :: res2, res2a
	integer :: res3, res3a
	integer :: res, resB

	call SumPOPCNT_internal(1,10)
	res1 = res

	call SumPOPCNT_internal(POPCNT(1),POPCNT(10))
	res2 = res

	call SumPOPCNT_internal(2,POPCNT(10))
	res3 = res

	if(res1 /= 3) ERROR STOP 1
	if(res2 /= 2) ERROR STOP 2
	if(res3 /= 2) ERROR STOP 3

	call SumPOPCNT_external(1,10,resB)
	res1a = resB

	call SumPOPCNT_external(POPCNT(1),POPCNT(10),resB)
	res2a = resB

	call SumPOPCNT_external(2,POPCNT(10),resB)
	res3a = resB

	if(res1a /= 3) ERROR STOP 4
	if(res2a /= 2) ERROR STOP 5
	if(res3a /= 2) ERROR STOP 6

	contains

	subroutine SumPOPCNT_internal(i,j)

		integer :: i, j
		integer :: i1, j1

		i1 = POPCNT(i)
		j1 = POPCNT(j)

		res = i1+j1

	end subroutine SumPOPCNT_internal

end program POPCNT_004f

subroutine SumPOPCNT_external(i,j, resA)

		implicit none

		integer, intent(in) :: i, j
		integer :: i1, j1
		integer, intent(out) :: resA

		i1 = POPCNT(i)
		j1 = POPCNT(j)

		resA = i1+j1

end subroutine SumPOPCNT_external
