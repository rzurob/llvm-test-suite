!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jul. 09, 2008
!*  PRIMARY FUNCTIONS TESTED   : Constraint C619 (R617)
!*  REFERENCE                  : Feature Number 353925
!*
!*  DESCRIPTION:
!*  Tests constraint C619 (R617) in a PDT:
!*  Array-section: if a substring range appears, the rightmost part-name shall
!*  be of type character.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

type dt1(n1)
    integer, len :: n1
    integer(4) :: i
    character(n1) :: c
end type

type dt2(n2)
    integer, len :: n2
    type(dt1(n2)) :: arr2(n2:n2+1)
    integer :: i
    character(n2) :: c
end type

type (dt2(3)) array(2)
type (dt2(:)), allocatable :: alloc_array(:)
allocate(dt2(3)  :: alloc_array(2))

! Error:
array(1)%i(:) = 1
array(1)%c(:) = 'abc'
! OK:
alloc_array(1)%c(:) = 'abc'

! Error:
array(1)%arr2(4)%i(:) = 1
array(1)%arr2(4)%c(:) = 'abc'
! OK:
alloc_array(1)%arr2(4)%c(:) = 'abc'

end program
