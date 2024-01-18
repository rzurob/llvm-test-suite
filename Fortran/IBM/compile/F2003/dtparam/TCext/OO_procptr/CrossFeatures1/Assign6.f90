! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qdeferredlp -qreuse=self /tstdev/OO_procptr/CrossFeatures1/Assign6.f
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
!*  defined op on proc ptr - illegal
!*  (ICE-306492)(306935)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

    INTERFACE
      FUNCTION CToC(Arg)
       CHARACTER(*) :: Arg
       CHARACTER(LEN(Arg)) :: CToc
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

    CONTAINS

    FUNCTION Fun(Arg)
    CHARACTER(*) :: Arg
    CHARACTER(LEN(Arg)) :: Fun
      Fun = Arg
    END FUNCTION

    FUNCTION MyAdd (Arg1, Arg2)
    PROCEDURE(CToC), POINTER, INTENT(IN) :: Arg1
    PROCEDURE(CToC), POINTER, INTENT(IN) :: Arg2
    PROCEDURE(CToC), POINTER :: MyAdd

    IF ( ASSOCIATED(Arg1)) THEN
      MyAdd => Arg1
    ELSE
      MyAdd => Arg2
    END IF

    END FUNCTION

  END MODULE


  PROGRAM Assign6
  USE M
  IMPLICIT NONE

  INTERFACE OPERATOR ( + )
    MODULE PROCEDURE  MyAdd
  END INTERFACE OPERATOr ( + )

  TYPE (DT(20,4)) :: V
  PROCEDURE(CToC), POINTER :: ProcPtr

  ProcPtr => RetPtr(Fun) + NULL(ProcPtr)
! IF (ProcPtr("ABC") .NE. "ABC" ) ERROR STOP 14

  ProcPtr => NULL(ProcPtr) + RetPtr(Fun)
! IF (ProcPtr("123") .NE. "123" ) ERROR STOP 15

  ProcPtr => RetPtr(Fun) + RetPtr(Fun)
! IF (ProcPtr("0123456789") .NE. "0123456789" ) ERROR STOP 16

  ProcPtr => RetPtr(NULL(Fun)) + RetPtr(Fun)
! IF (ProcPtr("xyz") .NE. "xyz" ) ERROR STOP 17

  ProcPtr => NULL(ProcPtr) + RetPtr(NULL(ProcPtr))
! IF (ASSOCIATED(ProcPtr)) ERROR STOP 18

  V%BComp%ProcPtr => RetPtr(Fun) + NULL(ProcPtr)
! IF (V%BComp%ProcPtr("ABC") .NE. "ABC" ) ERROR STOP 24

  V%BComp%ProcPtr => NULL(ProcPtr) + RetPtr(ProcPtr)
! IF (V%BComp%ProcPtr("123") .NE. "123" ) ERROR STOP 25

  V%BComp%ProcPtr => RetPtr(Fun) + RetPtr(Fun)
! IF (V%BComp%ProcPtr("0123456789") .NE. "0123456789" ) ERROR STOP 26

  V%BComp%ProcPtr => RetPtr(NULL(ProcPtr)) + RetPtr(Fun)
! IF (V%BComp%ProcPtr("xyz") .NE. "xyz" ) ERROR STOP 27

  V%BComp%ProcPtr => NULL(ProcPtr) + RetPtr(NULL(ProcPtr))
! IF (ASSOCIATED(V%BComp%ProcPtr)) ERROR STOP 28

  V%BComp%ProcPtr => RetPtr(Fun) + NULL(ProcPtr)
! IF (V%BComp%ProcPtr("ABC") .NE. "ABC" ) ERROR STOP 24

  V%BComp%ProcPtr => NULL(ProcPtr) + RetPtr(Fun)
! IF (V%BComp%ProcPtr("123") .NE. "123" ) ERROR STOP 25

  V%BComp%ProcPtr => RetPtr(Fun) + RetPtr(Fun)
! IF (V%BComp%ProcPtr("0123456789") .NE. "0123456789" ) ERROR STOP 26

  V%BComp%ProcPtr => RetPtr(NULL(ProcPtr)) + RetPtr(Fun)
! IF (V%BComp%ProcPtr("xyz") .NE. "xyz" ) ERROR STOP 27

  V%BComp%ProcPtr => NULL(ProcPtr) + RetPtr(NULL(ProcPtr))
! IF (ASSOCIATED(V%BComp%ProcPtr)) ERROR STOP 28


  CONTAINS

  FUNCTION RetPtr(Arg)
  PROCEDURE(CToC), POINTER :: RetPtr
  PROCEDURE(CToC) :: Arg
    RetPtr => Arg
  END FUNCTION

  END

