! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qdeferredlp -qreuse=self /tstdev/OO_procptr/CrossFeatures1/Assign2.f
! opt variations: -qnok -qnol -qdefaultpv -qnodeferredlp -qreuse=none

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : May. 16, 2005
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
!*  A derived-type intrinsic assignment
!*  (Coredump)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

    INTERFACE
      FUNCTION CToC(Arg)
       CHARACTER(*) :: Arg
       CHARACTER(LEN(Arg)) ::CToC
      END FUNCTION
    END INTERFACE

    TYPE :: Base(K1,N1)    ! (4,20)
      INTEGER, KIND :: K1
      INTEGER, LEN  :: N1
      PROCEDURE(CToC), NOPASS, POINTER :: ProcPtr=>NULL()
    END TYPE

    TYPE :: DT(N2,K2)    ! (20,4)
      INTEGER, KIND             :: K2
      INTEGER, LEN              :: N2
      INTEGER(K2)               :: Id
      TYPE(Base(K2,:)), POINTER :: BComp
    END TYPE

    INTERFACE ASSIGNMENT ( = )
      MODULE PROCEDURE PToP
    END INTERFACE ASSIGNMENT ( = )

    CONTAINS

    FUNCTION Fun(Arg)
    CHARACTER(*) :: Arg
    CHARACTER(LEN(Arg)) :: Fun
      Fun = Arg
    END FUNCTION

    SUBROUTINE PToP (Arg1, Arg2)
    TYPE(DT(*,4)), INTENT (OUT) :: Arg1
    TYPE(DT(*,4)), INTENT (IN)  :: Arg2
      Arg1%Id  = Arg2%Id
      Arg1%BComp => Arg2%BComp
    END SUBROUTINE

  END MODULE


  PROGRAM Assign2
  USE M
  IMPLICIT NONE

  TYPE (DT(20,4))                :: V
  TYPE (Base(4,20)), TARGET      :: BTar
  PROCEDURE(CToC), POINTER :: ProcPtr
  CHARACTER(1000000)       :: Str=CHAR(40)

  BTar = Base(4,20)(Fun)
  ProcPtr => Fun
  V = DT(20,4)(-1, BTar)

  IF ( V%Id .NE. -1 ) ERROR STOP 11
  IF ( .NOT. ASSOCIATED(V%BComp%ProcPtr, Fun) ) ERROR STOP 13

  IF (V%BComp%ProcPtr("ABC") .NE. "ABC" ) ERROR STOP 14

  IF (V%BComp%ProcPtr("")   .NE. "" )     ERROR STOP 15

  IF (V%BComp%ProcPtr(Str)   .NE. Str )   ERROR STOP 15


  END

