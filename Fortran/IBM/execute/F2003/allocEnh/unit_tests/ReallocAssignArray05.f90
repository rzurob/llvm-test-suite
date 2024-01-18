! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : June 5, 2006
!*
!*  DESCRIPTION                : Testing reallocation on assignment.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

logical(2), allocatable :: a(:)
allocate(a(3))
a = (/.true.,.false.,.true.,.false./)
if (any(shape(a) /= (/4/))) error stop 1
if (any(a .neqv. (/.true., .false., .true.,.false./))) error stop 2
end
