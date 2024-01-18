!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : F2003/dtparam/ArraySections/d356389.f
!*  DATE                       : Sep. 26, 2008
!*  PRIMARY FUNCTIONS TESTED   : See defect 356389
!*  REFERENCE                  : Feature Number 353925
!*
!*  DESCRIPTION:
!*  Source code for defect 356389
!*
!234567890123456789012345678901234567890123456789012345678901234567890

type base(n)
    integer, len :: n
    integer :: arr(n)   ! OK without this component, of if moved after id
    integer :: id
end type

type(base(:)), allocatable :: dtarrobj(:)
allocate(base(2) :: dtarrobj(3))

dtarrobj(:)%id = [(i,i=1,3)]
print *, dtarrobj(:)%id

end
