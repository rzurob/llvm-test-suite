!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2006-11-24
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : array construction intrinsics
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Use array construction functions not used elsewhere.  We're already using
!*  RESHAPE in many places, but no others (MERGE has been used, but not in its
!*  array form).  Here we test:
!*     CSHIFT    (array, shift [,dim])           Circular shift
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789


program acetint60cshift

  implicit none
  integer :: array(9), i

  if (any(cshift ([integer:: 1,2,3,4,5,6,7,8,9], 2) /= [integer:: 3,4,5,6,7,8,9,1,2])) error stop 2
  array = cshift ([integer:: 1,4,9,16,25,36,49,64,81], -1)
  if (any(array /= [integer:: 81, 1,4,9,16,25,36,49,64])) error stop 3
  print *, cshift ([integer:: 9,8,7,6,5,4,3,2,1], -2)

  array = [integer:: 1,2,3,4,5,6,7,8,9]
  array = [integer:: cshift(array,shift=3)]
  if (any(array /= [integer:: 4,5,6,7,8,9, 1,2,3])) error stop 4
  array = [integer:: 1,2,3,4,5,6,7,8,9]
  array = [integer:: cshift(reshape(array,[integer:: 3,3]), shift=[integer:: -1, 1, 0], dim=2)]
  if (any(array /= [integer:: 7,5,3, 1,8,6, 4,2,9])) error stop 5

  array = [integer:: 1,2,3,4,5,6,7,8,9]
  array = [integer:: cshift(reshape(array,[integer:: 3,3]), shift=[integer:: array(1:8:3)], dim=2)]
  if (any(array /= [integer:: 4,5,6, 7,8,9, 1,2,3])) error stop 6

  if (any(cshift ([integer:: (i,i=1,9)], 2) /= [integer:: (i,i=3,9), 1, 2])) error stop 2
  array = cshift ([integer:: (i ** 2, i=1,9)], -1)
  if (any(array /= [integer:: 9**2, (i ** 2, i=1,8)])) error stop 7
  print *, cshift ([integer:: (i, i=9,1,-1)], -2)

  array = [integer:: (i, i=1,9)]
  array = [integer:: cshift(array,shift=3)]
  if (any(array /= [integer:: (i, i=4,9), (i, i=1,3)])) error stop 8

end program acetint60cshift