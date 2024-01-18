! F2003/dtparam/ArraySections/d354613.f
! For defect 354613

type base(n1)
    integer, len :: n1
    integer :: id
end type

type pdt(n2)
    integer, len :: n2
    type(base(n2)) :: bc(n2)
end type

! OK if:
!type(pdt(3)) :: dta(2)

! seg faults if deferred-shape array:
type(pdt(3)), allocatable :: dta(:)
allocate(dta(2))

dta(1)%bc%id = (/1,2,3/)

! OK:
! do i=1,3
!    print *,dta(1)%bc(i)%id
! end do

print *,dta(1)%bc%id        ! seg faults
end
