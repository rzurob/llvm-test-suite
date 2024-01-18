type base (l)
   integer, len :: l
   integer i(l)
end type

type container (l1)
   integer, len :: l1
   type(base(l1)) :: b1
end type

type(container(:)), allocatable ::  c1(:), c2(:)
type(base(:)), allocatable ::  bbb(:)
allocate(container(2) :: c1(3))
allocate(bbb, source=c1%b1)
print*, lbound(bbb)
print*, ubound(bbb)
end
