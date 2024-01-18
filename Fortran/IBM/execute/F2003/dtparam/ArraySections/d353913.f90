!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : F2003/dtparam/ArraySections/d353913.f
!*  PROGRAMMER                 : Gaby Baghdadi
!*  DATE                       : Jul. 09, 2008
!*  ORIGIN                     : Compiler Development, IBM Toronto Lab
!*  PRIMARY FUNCTIONS TESTED   : See defect 353913
!*  REFERENCE                  : Feature Number 353925
!*
!*  DESCRIPTION:
!*  Source code for defect 353913
!*
!234567890123456789012345678901234567890123456789012345678901234567890

type dt(n)
    integer, len :: n
    character(n) :: arr       ! seg faults with this
!   character(4) :: arr       ! runs fine with this instead
end type

type (dt(4)) :: xdt

type (dt(:)), allocatable :: adt
allocate(dt(4) :: adt)

! this is OK:
xdt%arr(1:4) = 'abcd'
print *,'1:',xdt%arr(1:4)

! allocate OK:
print *,'2:',len(adt%arr)

! no substring-range - OK:
adt%arr = 'abcd'
print *,'3:',adt%arr

! any of the 4 stmts below would seg fault:
adt%arr(1:4) = 'abcd'
print *,'4:',adt%arr(1:4)

adt%arr(:) = 'abcd'
print *,'5:',adt%arr(:)

end
