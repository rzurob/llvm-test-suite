! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : June 5, 2006
!*
!*  DESCRIPTION                : Testing reallocation on assignment,
!*                               with a scalar, deferred-length
!*                               character on the left-hand side.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

character(:), allocatable :: a
character(5) :: b
b = 'abcde'
allocate(character(40) :: a)
a = b(3:4)
if (len(a) /= 2) then
  print *, len(a)
  stop 1
end if
end
