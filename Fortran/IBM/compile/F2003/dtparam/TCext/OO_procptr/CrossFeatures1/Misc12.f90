! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/OO_procptr/CrossFeatures1/Misc12.f
! opt variations: -qnok -qnol

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jun. 08, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure pointer
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature 289058
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  ASSOCIATE/SELECT TYPE
!*
!*  (306669)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM Misc12
  IMPLICIT NONE

  TYPE :: DT(K1,N1)    ! (4,20)
    INTEGER, KIND :: K1
    INTEGER, LEN  :: N1
    PROCEDURE(), POINTER, NOPASS :: ProcPtr => NULL()
  END TYPE

  PROCEDURE(TYPE(DT(4,20))), POINTER :: ProcPtr
  PROCEDURE(),               POINTER :: ProcPtr1

  TYPE(DT(4,20))  :: V

  ASSOCIATE ( As => ProcPtr )
  END ASSOCIATE

  ASSOCIATE ( As => V%ProcPtr )
  END ASSOCIATE

  ASSOCIATE ( As => ProcPtr1 )
  END ASSOCIATE

  ASSOCIATE ( As => ProcPtr() )
  END ASSOCIATE

! SELECT TYPE (As => ProcPtr)
! END SELECT

! SELECT TYPE (As => V%ProcPtr)
! END SELECT

  END


