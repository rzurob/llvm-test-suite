! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/associate/StmtByStmt/Exec/HostAssocConstStruct1.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 02, 2004
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
!*    The selector is an associte name associating to a constant structure (component)
!*    Test if a const can be changed by type bound procedures
!*   (Comp failed)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
    TYPE :: Base(K1)    ! (4)
      INTEGER, KIND :: K1
      INTEGER(K1)   :: BaseId = 1
    CONTAINS
      PROCEDURE, PASS   :: SetId => SetBaseId
    END TYPE
  CONTAINS
    SUBROUTINE SetBaseId(Arg)
    CLASS(Base(4))  :: Arg
       Arg%BaseId = -1
    END SUBROUTINE

  END MODULE

  PROGRAM HostAssocConstStruct1
  USE M
  IMPLICIT NONE

  TYPE(Base(4)), PARAMETER :: W = Base(4)()
  ASSOCIATE (V=>W)
    PRINT*, V%BaseId
    CALL V%SetId
    PRINT*, V%BaseId
    IF ( V%BaseId .NE. -1 ) STOP 40
  END ASSOCIATE

  END

