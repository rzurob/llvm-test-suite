module modulePOPPAR

	implicit none

	integer(1), parameter :: i1=POPPAR(1)
	integer(1), parameter :: i2=POPPAR(10)
	integer(1), parameter :: i3=POPPAR(100)
	integer(1), parameter :: i4=POPPAR(1000)
	integer :: res

	contains

	integer function SumPOPPAR(i,j)

		integer :: i, j
		integer :: i1, j1

		i1 = POPPAR(i)
		j1 = POPPAR(j)

		SumPOPPAR = i1+j1

	end function SumPOPPAR

	subroutine Sum(i,j)

		integer :: i, j
		integer :: i1, j1

		i1 = POPPAR(i)
		j1 = POPPAR(j)

		res = i1+j1

	end subroutine Sum

end module modulePOPPAR
