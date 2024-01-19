! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qdeferredlp -qreuse=self /tstdev/OO_procptr/CrossFeatures1/StrComp.f
! opt variations: -qnok -qnol -qdefaultpv -qnodeferredlp -qreuse=none

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : May. 17, 2005
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
!*  Structure component
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M


    TYPE :: Base(K1,N1)    ! (4,20)
      INTEGER, KIND :: K1
      INTEGER, LEN  :: N1
      PROCEDURE(COMPLEX), NOPASS, POINTER :: ProcPtr=>NULL()
    END TYPE

    TYPE :: DT(N2,K2)    ! (20,4)
      INTEGER, KIND             :: K2
      INTEGER, LEN              :: N2
      INTEGER(K2)               :: Id=0
      TYPE(Base(K2,:)), POINTER :: BComp
    END TYPE


    CONTAINS

    FUNCTION Fun(Arg)
    COMPLEX :: Arg
    COMPLEX :: Fun
      Fun = Arg
    END FUNCTION


  END MODULE


  PROGRAM StrComp
  USE M
  IMPLICIT NONE

  TYPE(DT(20,4))   :: D(512)

  D%BComp%ProcPtr => Fun

  D(1::2)%BComp%ProcPtr => Fun

  D(2:2)%BComp%ProcPtr => Fun

  D(3:2)%BComp%ProcPtr => Fun

  END

