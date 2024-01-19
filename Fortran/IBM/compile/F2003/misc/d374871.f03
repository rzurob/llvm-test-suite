!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2010-02-04
!*
!*  PRIMARY FUNCTIONS TESTED   : misc
!*
!*  SECONDARY FUNCTIONS TESTED : catch defect 374871 (ICE on def.init. use of missing DTP type)
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
!*  As part of an attempted fix to unexpected output from dtpIAABasicFunction003 (ultimately defect 367675),
!*  this code ICEd the compiler, generating a SIGTRAP in procedure __vc__13ParamArray1dUXT7Fedic_tSP4194304_CFl,
!*  invoked from procedure p2_fdcl (see the defect for more details).
!*  It should, of course, simply generate an error message.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

type container
   type(dk(4)) :: dkvar = dk(4)()
end type container
end
