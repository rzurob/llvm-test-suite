! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : November 30, 2010
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Test that POPPAR can be used as
!*				 constant expression in an
!*				 initialization expressioan (in subroutine)
!*
!*  TARGET(S)                  :
!* ===================================================================
program POPPAR_004f

	implicit none

	interface
    subroutine SumPOPPAR_external(i,j, resA)
     integer, INTENT(IN) :: i,j
     integer, INTENT(OUT) :: resA
    end subroutine SumPOPPAR_external
    end interface

	integer :: res1, res1a
	integer :: res2, res2a
	integer :: res3, res3a
	integer :: res, resB

	call SumPOPPAR_internal(1,10)
	res1 = res

	call SumPOPPAR_internal(POPPAR(1),POPPAR(10))
	res2 = res

	call SumPOPPAR_internal(2,POPPAR(10))
	res3 = res

	if(res1 /= 1) ERROR STOP 1
	if(res2 /= 1) ERROR STOP 2
	if(res3 /= 1) ERROR STOP 3

	call SumPOPPAR_external(1,10,resB)
	res1a = resB

	call SumPOPPAR_external(POPPAR(1),POPPAR(10),resB)
	res2a = resB

	call SumPOPPAR_external(2,POPPAR(10),resB)
	res3a = resB

	if(res1a /= 1) ERROR STOP 4
	if(res2a /= 1) ERROR STOP 5
	if(res3a /= 1) ERROR STOP 6

	contains

	subroutine SumPOPPAR_internal(i,j)

		integer :: i, j
		integer :: i1, j1

		i1 = POPPAR(i)
		j1 = POPPAR(j)

		res = i1+j1

	end subroutine SumPOPPAR_internal

end program POPPAR_004f

subroutine SumPOPPAR_external(i,j, resA)

		implicit none

		integer, intent(in) :: i, j
		integer :: i1, j1
		integer, intent(out) :: resA

		i1 = POPPAR(i)
		j1 = POPPAR(j)

		resA = i1+j1

end subroutine SumPOPPAR_external
