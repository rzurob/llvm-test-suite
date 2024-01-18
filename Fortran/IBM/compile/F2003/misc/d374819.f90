!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : d374819
!*
!*  DATE                       : 2010-02-03
!*
!*  PRIMARY FUNCTIONS TESTED   : misc
!*
!*  SECONDARY FUNCTIONS TESTED : catch defect 374819 (FEPLCAT problem)
!*
!*  REFERENCE                  : defect 367675
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*  ADAPTED FROM               : dtpIAABasicFunction003 (dtpIAABasic003<-dtpIAABasic002<-dtpIAABasic001)
!*
!*  DESCRIPTION
!*
!*  As part of an attempted fix to unexpected output from dtpIAABasicFunction003,
!*  this code ICEd the compiler with "1517-005 (U) Error in FEPLCAT: second list neither null nor header."
!*  It should, of course, simply generate an error message, indicating that the default value used in
!*  the declaration for "container" is must have a length parameter known at compile time.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpIAABasicFunction003mod

  type dl(l)
     integer, len :: l
     character(l) :: chvar = ''
  end type dl

  type container(l1)
     integer, len  :: l1
     type(dl(l1)) :: dlvar = dl(l1)()
  end type container

  type(container(2)), save :: c

end module dtpIAABasicFunction003mod
