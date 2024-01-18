!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : F2003/dtparam/ArraySections/c618d1.f
!*  PROGRAMMER                 : Gaby Baghdadi
!*  DATE                       : Jul. 09, 2008
!*  ORIGIN                     : Compiler Development, IBM Toronto Lab
!*  PRIMARY FUNCTIONS TESTED   : Constraint C618 (R617)
!*  REFERENCE                  : Feature Number 353925
!*
!*  DESCRIPTION:
!*
!*  Tests constraint C618 (R617) in a PDT with array components having scalar 
!*  size: 
!*  In an array-section, exactly one 
!*  part-ref shall have nonzero rank, and either the final part-ref shall have 
!*  a section-subscript-list with nonzero rank or another part-ref shall have 
!*  nonzero rank.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

type dt1(k1)
    integer, kind :: k1
    integer(k1) :: arr1(3)
end type

type dt2(k2)
    integer, kind :: k2
    type(dt1(k2)) arr2(3,2)
end type

type (dt2(4)) array(4)

!* In an array-section, exactly one part-ref must have nonzero rank...

!* and either the final part-ref has a section-subscript-list with nonzero rank:
array(2)%arr2(3,1)%arr1(:) = (/ -1,0,1 /)  ! OK
array(2)%arr2(3,1)%arr1(1:3) = (/ -1,0,1 /)  ! OK

!* or another part-ref has nonzero rank:
array(2)%arr2(:,1)%arr1(3) = (/ 2,3,4 /)   ! OK

!* or another part-ref has nonzero rank:
array(2)%arr2(:,:)%arr1(3) = reshape((/ (i,i=1,6) /),(/ 3,2 /))   ! OK

!* Compiler should complain here:
array(2)%arr2(:,1)%arr1(:) = (/ (i,i=1,9) /)

end program
