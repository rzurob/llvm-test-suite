!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2006-11-24
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : array construction intrinsics - spread
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
!*     SPREAD    (source, dim, ncopies)          Replicates array by adding a dimension
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789


program acetint60spread

  implicit none
  integer :: array(9), i

  array = spread(1, 1, 9)
  if (any(array /= 1)) error stop 2

  array = [integer:: spread(1, 1, 9)]
  if (any(array /= 1)) error stop 3

  print *, spread([integer:: 1,2,3], 1, 3)
  array = [integer:: spread([integer:: 1,2,3], 1, 3)]
  if (any(array /= [integer:: (i,i,i,i=1,3)])) error stop 4

  print *, spread([integer:: (i, i=1,3)], 1, 3)
  array = [integer:: spread([integer:: (i, i=1,3)], 1, 3)]
  if (any(array /= [integer:: (i,i,i,i=1,3)])) error stop 5

end program acetint60spread