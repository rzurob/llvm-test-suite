! GB DTP extension using:
! ftcx_dtp -qck /tstdev/OO_poly/selectType/CrossFeatures/SelectType2.f
! opt variations: -qnock

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: SelectType2.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : SelectCase2
!*
!*  DATE                       : Feb. 04, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type
!*
!*  SECONDARY FUNCTIONS TESTED : Selector
!*
!*  REFERENCE                  : Feature 219934.OO_poly
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!* Select Type
!* ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM SelectType2
  IMPLICIT CLASS(*)(U)
  TYPE :: DT(K1,K2,N1)    ! (4,1,30000)
    INTEGER, KIND             :: K1,K2
    INTEGER, LEN              :: N1
    INTEGER(K1)               :: Int
    CHARACTER(kind=K2,len=N1) :: C
  END TYPE
  INTEGER :: i

  CALL Sub(DT(4,1,30000)(Int=6, C="!"))

  CONTAINS

  SUBROUTINE Sub(U)

  ASSOCIATE ( U => U)
    SELECT TYPE (U)
    CLASS IS (DT(4,1,*))
      ASSOCIATE (U => U)
        SELECT TYPE(U)
        CLASS DEFAULT
          ASSOCIATE ( U => U)
            IF ( U%Int .NE. 6 )       STOP 22
            IF ( TRIM(U%C) .NE. "!" ) STOP 23
          END ASSOCIATE
        END SELECT
      END ASSOCIATE

    CLASS DEFAULT
      STOP 40
    END SELECT
  END ASSOCIATE

  END SUBROUTINE

  END



