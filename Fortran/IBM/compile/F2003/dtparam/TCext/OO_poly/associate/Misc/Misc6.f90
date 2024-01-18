! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/associate/Misc/Misc6.f
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
!*  Missing actual argument in associate selector caused ICE
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE :: Base(K1,N1)    ! (4,20)
        INTEGER, KIND :: K1
        INTEGER, LEN  :: N1
    CONTAINS
      PROCEDURE, NoPASS   :: ReturnBase
    END TYPE

    CONTAINS

    FUNCTION ReturnBase(Arg)
    CLASS(Base(4,*)) :: Arg
    CLASS(Base(4,:)), ALLOCATABLE  :: ReturnBase
      ALLOCATE(ReturnBase, SOURCE=Arg)
    END FUNCTION

  END MODULE

  PROGRAM  Misc6
  USE M
  IMPLICIT NONE

  ASSOCIATE (As => ReturnBase()) !Missing argument here
    PRINT*, SAME_TYPE_AS(As, Base(4,20)())
  END ASSOCIATE

  END

