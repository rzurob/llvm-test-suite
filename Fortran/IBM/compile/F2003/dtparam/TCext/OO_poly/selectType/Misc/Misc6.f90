! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/OO_poly/selectType/Misc/Misc6.f
! opt variations: -qnok -qnol

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Dec. 16, 2004
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type
!*
!*  SECONDARY FUNCTIONS TESTED :
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
!*  Non poly entity of implicit type
!*    (ICE)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
    TYPE :: DT(K1,N1)    ! (4,20)
        INTEGER, KIND :: K1
        INTEGER, LEN  :: N1
    END TYPE

    CONTAINS

    SUBROUTINE Sub(CArg)
    IMPLICIT TYPE(DT(4,20))(C)

      SELECT TYPE (CArg)
        TYPE IS(DT(4,*))
      END SELECT
    END SUBROUTINE

  END MODULE

  PROGRAM  Misc6
  END

