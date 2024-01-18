module modulePOPCNT
	
	implicit none
	
	integer(1), parameter :: i1=POPCNT(1)
	integer(1), parameter :: i2=POPCNT(10)
	integer(1), parameter :: i3=POPCNT(100)
	integer(1), parameter :: i4=POPCNT(1000)
	integer :: res	

	contains
	
	integer function SumPOPCNT(i,j)
		
		integer :: i, j
		integer :: i1, j1
		
		i1 = POPCNT(i)
		j1 = POPCNT(j)
		
		SumPOPCNT = i1+j1
			
	end function SumPOPCNT	
	
	subroutine Sum(i,j)
		
		integer :: i, j
		integer :: i1, j1
				
		i1 = POPCNT(i)
		j1 = POPCNT(j)
				
		res = i1+j1
		
	end subroutine Sum
	
end module modulePOPCNT
