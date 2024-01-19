!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2006-11-24
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : array construction intrinsics - merge
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
!*  RESHAPE in many places; MERGE has also been used, but not in its array
!*  form (e.g., merge(1,2,log) is 1 if log is true, and otherwise 2).  We
!*  have not used these yet:
!*     CSHIFT    (array, shift [,dim])           Circular shift
!*     EOSHIFT   (array, shift [,boundary, dim]) End-off shift
!*     MERGE     (tsource, fsource, mask)        Merge under mask
!*     PACK      (array, mask [,vector])         Pack into an array of rank one under a mask
!*     SPREAD    (source, dim, ncopies)          Replicates array by adding a dimension
!*     TRANSPOSE (matrix)                        Transpose a rank-2 array
!*     UNPACK    (vector, mask, field)           inverse of pack
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789


program acetint60merge

  implicit none
  integer :: array(9), control5(5), control9(9), i
  logical, parameter :: T = .true., F = .false.

  control5 = [integer:: 1, 0, 1, 1, 0]
  control9 = [integer:: 1, 0, 1, 1, 0, 0, 0, 1, 1]

  print *, merge([integer:: 1,2,3,4,5], [integer:: 1, 4, 9, 16, 25], [logical:: (control5(i) == 1, i=1,5)]) ! 1 4 3 4 25
  if (any(merge([integer:: 1, 2, 3, 4, 5], [integer:: 1, 4, 9, 16, 25], [logical:: (control5(i) == 1, i=1,5)]) /= [integer:: 1, 4, 3, 4, 25])) error stop 2
  array = [integer:: 1, 2, 3, 4, 5, 6, 7, 8, 9]
  array = merge([integer:: 1, 2, 3, 4, 5, 6, 7, 8, 9], [integer:: 81, 64, 49, 36, 25, 16, 9, 4, 1], [logical:: (control9(i) == 1, i=1,9)])
  if (any(array /= [integer:: 1, 64, 3, 4, 25, 16, 9, 8, 9])) error stop 3

  print *, merge([integer:: (i, i=1,5)], [integer:: (i ** 2, i=1,5)], [logical:: T, F, T, T, F]) ! 1 4 3 4 25
  if (any(merge([integer:: (i, i=1,5)], [integer:: (i ** 2, i=1,5)], [logical:: T, F, T, T, F]) /= [integer:: 1, 4, 3, 4, 25])) error stop 4
  array = [integer:: (i, i=1,9)]
  array = merge([integer:: (array(i), i=1,9)], [integer:: (array(i) ** 2, i=9,1,-1)], [logical:: T, F, T, T, F, F, F, T, T])
  if (any(array /= [integer:: 1, 64, 3, 4, 25, 16, 9, 8, 9])) error stop 5

end program acetint60merge
