!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : acetint61
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2006-11-26
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : rank remapped pointers in AC
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : 
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 
!*
!*  DESCRIPTION
!*
!*  Define a matrix via a rank remapped pointer and an AC.
!*  Use same in an AC.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program acetint61

  implicit none
  real, allocatable, target :: base(:)
  real, pointer :: matrix(:,:), diagonal(:)
  integer :: n, i

  ! "pointer rank remapping"
  n = 6
  allocate(base(n*n))
  matrix(1:n,1:n) => base ! rank remapping
  diagonal => base(::n+1)

  base = [real:: (real(i/10.0), i = 1,36)]
  diagonal = [real:: (real(i*i), i=1,n)]
  print *, base

  diagonal = [real:: (diagonal(i), i=n,1,-1)]
  print *, base

  matrix(3,:) = [real:: matrix(2,:)]
  print *, base

  matrix(2,:) = [real:: (matrix(1,i), i=1,n)]
  print *, base

  matrix(4,:) = [real:: diagonal]
  print *, base

  diagonal = [real:: (sum(matrix(i,:)), i=1,6)]
  print *, base

  diagonal = [real:: (sum(matrix(:,i)), i=1,6)]
  print *, base

end program acetint61
