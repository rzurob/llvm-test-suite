!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2006-11-24
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : array construction intrinsics - transpose
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
!*     TRANSPOSE (matrix)                        Transpose a rank-2 array
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789


program acetint60transpose

  implicit none
  integer :: array(9), array19(1,9), array91(9,1), array33(3,3), i

  array19 = reshape([integer:: (i, i=1,9)], [1,9])
  array91 = transpose (array19)
  if (any([integer:: array19] /= [integer:: array91])) stop 2

  array33 = reshape([integer:: (i, i=1,9)], [3,3])
  array33 = transpose(array33)
  if (any([integer:: array33] /= [integer:: 1,4,7,2,5,8,3,6,9])) stop 3
  print *, array33
  array33 = transpose(reshape([integer:: (i, i=1,9)], [3,3]))
  if (any([integer:: array33] /= [integer:: 1,4,7,2,5,8,3,6,9])) stop 4
  if (any([integer:: transpose(reshape([integer:: (i, i=1,9)], [3,3]))] /= [integer:: 1,4,7,2,5,8,3,6,9])) stop 5

  array91 = transpose(reshape([integer:: (i, i=1,9)], [1,9]))
  print *, array91
  print *, transpose(reshape([integer:: (i, i=1,9)], [1,9]))

  if (any([integer:: transpose(reshape([integer:: 1,4,7,2,5,8,3,6,9], [3,3]))] /= [integer:: (i, i=1,9)])) stop 6

  if (any([integer:: transpose(transpose(reshape([integer:: (i, i=1,9)], [3,3])))] /= [integer:: (i, i=1,9)])) stop 7

  if (any([integer:: transpose(transpose(reshape([integer:: (i, i=1,1024)], [8,128])))] /= [integer:: (i, i=1,1024)])) stop 8

end program acetint60transpose
