! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/associate/Misc/Misc26.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb. 16, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Associate
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature 219934
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Dev complains the init expre
!*  (300174)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM Misc26


  TYPE :: Base(K1)    ! (4)
    INTEGER, KIND :: K1
    INTEGER(K1)   :: BaseId = 1
  END TYPE

  integer :: i

  TYPE(Base(4))  :: V(3) = (/(Base(4)(i), i =1, 3)/)

  IF (ANY(V%BaseID  .NE. (/1,2,3/)) ) ERROR STOP 11

  END


