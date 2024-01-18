! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/associate/CrossFeatures/Where.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Mar. 09, 2005
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
!*    The where stmt
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
    TYPE :: DT(K1)    ! (4)
      INTEGER, KIND :: K1
      INTEGER(K1)   :: Id = 1
      CONTAINS
      PROCEDURE, PASS   :: GetId
    END TYPE

  CONTAINS

    ELEMENTAL FUNCTION GetId(Arg)
    IMPLICIT CLASS(DT(4))(A)
    INTENT(IN) :: Arg
    INTEGER    :: GetId
      GetId = Arg%Id
    END FUNCTION

  END MODULE

  PROGRAM Where

  USE M
  IMPLICIT TYPE(DT(4))(A)
  DIMENSION :: Arr(2:4)

  ASSOCIATE ( As => (/DT(4)(-3), DT(4)(-3), DT(4)(-3)/) )
    WHERE ((/.TRUE., .TRUE., .TRUE. /) )
      Arr(2:)  = As
    END WHERE
  END ASSOCIATE

  IF ( Any(Arr%ID      .NE. -3) ) ERROR STOP 60
  IF ( ANY(Arr%GetID() .NE. -3) ) ERROR STOP 61

  ASSOCIATE ( As => Arr )
    WHERE ((/.TRUE., .TRUE., .TRUE. /) )
      As%ID = -2
    END WHERE
  END ASSOCIATE

  IF ( Any(Arr%ID      .NE. -2) ) ERROR STOP 70
  IF ( ANY(Arr%GetID() .NE. -2) ) ERROR STOP 71

  ASSOCIATE ( As => Arr )
    WHERE ((/.TRUE., .TRUE., .TRUE. /) )
      As%ID = As%ID + 1
    END WHERE
  END ASSOCIATE

  IF ( Any(Arr%ID      .NE. -1) ) ERROR STOP 80
  IF ( ANY(Arr%GetID() .NE. -1) ) ERROR STOP 81

  ASSOCIATE ( As => Arr(:) )
    WHERE ( As(:)%ID < 0 )
      As%ID = As%ID + 5
    END WHERE
  END ASSOCIATE

  IF ( Any(Arr%ID      .NE. 4) ) ERROR STOP 90
  IF ( ANY(Arr%GetID() .NE. 4) ) ERROR STOP 91

  END


