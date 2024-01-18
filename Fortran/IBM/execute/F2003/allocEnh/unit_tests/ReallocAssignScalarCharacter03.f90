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
integer x
x = 4
b = 'abcde'
allocate(character(40) :: a)
a = b(1:x)
if (len(a) /= 4) stop 1
end
