!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2008-08-25
!*
!*  PRIMARY FUNCTIONS TESTED   : DTP and USE
!*
!*  SECONDARY FUNCTIONS TESTED : misc defects
!*
!*  REFERENCE                  : Feature Number 355310
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Test case for defect 356268, in which a parameterised DT component of
!*  deferred length is tested.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program dtpUseDeferredDT

type string(l)
   integer, len :: l
   character(l) :: s
end type string

type string_list
   type(string(:)) :: datum
end type string_list

type(string_list) :: sl
sl  = string_list(string(7)('jumping'))

end program dtpUseDeferredDT
