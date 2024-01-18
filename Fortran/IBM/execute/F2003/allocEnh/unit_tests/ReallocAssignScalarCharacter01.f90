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
a = b
if (len(a) /= 5) error stop 1
end
