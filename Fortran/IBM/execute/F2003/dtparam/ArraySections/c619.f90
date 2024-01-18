!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : F2003/dtparam/ArraySections/c619.f
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
    character :: arr1*3
    character(n1*2) :: arr2
end type

type dt2(n2)
    integer, len :: n2
    type(dt1(n2)) :: arr3(n2:n2+1)
end type

integer, parameter :: N=3
type (dt2(N)), dimension(5,4,3) :: array1

type (dt2(:)), dimension(:,:,:), allocatable :: array2
allocate(dt2(3) :: array2(5,4,3))

array1(3,4,2)%arr3(4)%arr1(1:3) = 'abc'
print *,array1(3,4,2)%arr3(4)%arr1(1:3)

array1(5,4,1)%arr3(4)%arr2(:) = 'abc'
print *,array1(5,4,1)%arr3(4)%arr2(:)

array2(3,4,2)%arr3(4)%arr1(1:3) = 'abc'
print *,array2(3,4,2)%arr3(4)%arr1(1:3)

array2(5,4,1)%arr3(4)%arr2(:) = 'abc'
print *,array2(5,4,1)%arr3(4)%arr2(:)

end program
