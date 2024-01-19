! F2003/dtparam/ArraySections/d354611.f
! For defect 354611

type base
	integer :: id
end type

type pdt(n)
	integer, len :: n
	type(base) :: bc(n)
end type

type(pdt(3)) :: dta(2)
dta(:)%bc(3)%id = (/1,2/)
print *,dta(:)%bc(3)%id

end
