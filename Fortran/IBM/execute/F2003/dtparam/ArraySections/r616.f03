!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jul. 09, 2008
!*  PRIMARY FUNCTIONS TESTED   : See Description below.
!*  REFERENCE                  : Feature Number 353925
!*
!*  DESCRIPTION:
!*  Tests R616 (array-element) and C617 (R616):
!*  Every part-ref shall have rank zero and the last part-ref shall contain a
!*  subscript-list
!*
!234567890123456789012345678901234567890123456789012345678901234567890

type dt0(n1)
    integer, len :: n1
    integer(8) arr1(n1,n1+1)
end type

type, extends(dt0) :: dt1
    complex c
end type

type dt2(n2)
    integer, len :: n2
    type(dt1(n2)) arr2(n2)
    real :: r
end type

integer, parameter :: N = 3
!type (dt2(N)) :: arrdt(10:12)
type (dt2(:)), dimension(10:12), allocatable :: arrdt(:)
allocate(dt2(N) :: arrdt(10:12));

! Array-element: every part-ref shall have rank zero and the last part-ref
! shall contain a subscript-list:
arrdt(10)%arr2(3)%arr1(2,3) = 5
print *,kind(arrdt(10)%arr2(3)%arr1(2,3))
print *,arrdt(10)%arr2(3)%arr1(2,3)

! if at least one part-ref has rank of non-zero, then it's not an array-element:
if (size(arrdt(11)%arr2(2:3)%arr1(2,1)) <> 2) error stop 1

! if last part-ref does not contain a subscript-list, it's not an array-element:
arrdt(12)%arr2(1)%arr1 = reshape((/ (i,i=1,N*(N+1)) /),(/ N,N+1 /))
print *,arrdt(12)%arr2(1)%arr1

end program
