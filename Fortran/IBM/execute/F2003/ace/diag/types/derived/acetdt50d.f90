!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : acetdt50d
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2006-11-16
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : incompatible AC in SOURCE of ALLOCATE - derived type
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : source, allocate, AC
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 
!*
!*  DESCRIPTION
!*
!*  Try to allocate several derived type arrays with an AC as the source, but
!*  using mismatching types.  Scatter a few implied-do's, too.
!*  (DTP is not covered here, so mismatched type parameters are elsewhere.)
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module acetdt50dmod

  implicit none
  type derived
  end type derived

  type other
  end type other

end module acetdt50dmod


program acetdt50d

  use acetdt50dmod
  implicit none
  type (derived), allocatable :: dtarr(:)
  type (other), allocatable   :: otarr(:)
  integer :: i

  ! This should be okay, for a baseline:
  allocate (dtarr(3), source=[derived:: derived(), derived(), derived()])
  deallocate(dtarr)
  allocate (dtarr(3), source=[derived:: (derived(), i=1,3)])
  deallocate(dtarr)

  ! These should be bad:
  allocate (dtarr(3), source=[other:: other(), other(), other()])
  deallocate(dtarr)
  allocate (dtarr(3), source=[other:: (other(), i=1,3)])
  deallocate(dtarr)
  allocate (dtarr(0), source=[character(2)::])
  deallocate(dtarr)
  allocate (dtarr(0), source=[character(2):: ('xx', i=1,0)])

end program acetdt50d
