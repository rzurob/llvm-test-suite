! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/associate/Misc/Misc5.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 12, 2004
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
!*    Array initialization failed when ac control is invloved
!*    (Complaint:  1516-050 (S) Expression or initial value must be evaluated at compile time.)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  PROGRAM Misc5

  TYPE :: Base(K1)    ! (4)
    INTEGER, KIND :: K1
    INTEGER(K1)   :: BaseId = 1
  END TYPE

  TYPE(Base(4)) :: V(3)= (/ (Base(4)(i), i=1,3)/)

  IF ( ANY (V%BaseId .NE. (/1,2,3/)) ) STOP 11

  END
