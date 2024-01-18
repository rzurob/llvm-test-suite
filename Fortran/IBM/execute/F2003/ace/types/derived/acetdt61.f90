!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : acetdt61
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

module acetdt61mod

  implicit none
  type dt
     real :: val
  end type dt

end module acetdt61mod


program acetdt61

  use acetdt61mod
  implicit none
  type(dt), allocatable, target :: base(:)
  type(dt), pointer :: matrix(:,:), diagonal(:)
  integer :: n, i

  ! "pointer rank remapping"
  n = 6
  allocate(base(n*n))
  matrix(1:n,1:n) => base ! rank remapping
  diagonal => base(::n+1)

  base = [dt:: (dt(real(i/10.0)), i = 1,36)]
  diagonal = [dt:: (dt(real(i*i)), i=1,n)]
  print *, base

  diagonal = [dt:: (diagonal(i), i=n,1,-1)]
  print *, base

  matrix(3,:) = [dt:: matrix(2,:)]
  print *, base

  matrix(2,:) = [dt:: (matrix(1,i), i=1,n)]
  print *, base

  matrix(4,:) = [dt:: diagonal]
  print *, base

  diagonal = [dt:: (dt(sum(matrix(i,:)%val)), i=1,6)]
  print *, base

  diagonal = [dt:: (dt(sum(matrix(:,i)%val)), i=1,6)]
  print *, base

end program acetdt61
