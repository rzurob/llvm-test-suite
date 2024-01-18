! GM DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp /tstdev/F2003/ace/diag/types/derived/acetdt50d.f

!***********************************************************************
!* =====================================================================
!*
!*  TEST CASE NAME             : acetdt50dkl_dlp
!*
!*                               by David Forster)
!*  DATE                       : 2007-11-30 (original: 2006-11-16)
!*  ORIGIN                     : Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*
!*  SECONDARY FUNCTIONS TESTED : Array Constructor Enhancements
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*
!*  REFERENCE                  : Feature Number 289053
!*
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
!23456789012345678901234567890123456789012345678901234567890123456789012

module acetdt50dkl_dlpmod

  implicit none
  type derived(k1,l1)    ! (4,20)
      integer, kind :: k1
      integer, len  :: l1
  end type derived

  type other(k2,l2)    ! (4,20)
      integer, kind :: k2
      integer, len  :: l2
  end type other

end module acetdt50dkl_dlpmod


program acetdt50dkl_dlp

  use acetdt50dkl_dlpmod
  implicit none
  type (derived(4,:)), allocatable :: dtarr(:)
  type (other(4,:)), allocatable   :: otarr(:)
  integer :: i

  ! This should be okay, for a baseline:
  allocate (dtarr(3), source=[derived:: derived(4,20)(), derived(4,20)(), derived(4,20)()])
  deallocate(dtarr)
  allocate (dtarr(3), source=[derived:: (derived(4,20)(), i=1,3)])
  deallocate(dtarr)

  ! These should be bad:
  allocate (dtarr(3), source=[other:: other(4,20)(), other(4,20)(), other(4,20)()])
  deallocate(dtarr)
  allocate (dtarr(3), source=[other:: (other(4,20)(), i=1,3)])
  deallocate(dtarr)
  allocate (dtarr(0), source=[character(2)::])
  deallocate(dtarr)
  allocate (dtarr(0), source=[character(2):: ('xx', i=1,0)])

end program acetdt50dkl_dlp
