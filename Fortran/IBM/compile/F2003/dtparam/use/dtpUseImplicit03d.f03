!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2008-08-25
!*
!*  PRIMARY FUNCTIONS TESTED   : DTP and USE
!*
!*  SECONDARY FUNCTIONS TESTED : USEd type in implicit statement
!*
!*  REFERENCE                  : Feature Number 355310
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : implicit
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Verify that error messages are correctly generated for USEd types in implicit
!*  statements, here with deferred length (should require POINTER or ALLOCATABLE).
!*  (This differs from dtpUseImplicit02d in that no component actually requires
!*  the length parameter.)
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789


module dtpUseModule

  implicit none
  type :: tl(l)
     integer, len  :: l
  end type tl

end module dtpUseModule


program dtpUseImplicit03d

  use :: dtpUseModule
  implicit type(tl(:))(s)

  print *, s1

end program dtpUseImplicit03d
