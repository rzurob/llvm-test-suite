! GB DTP extension using:
! ftcx_dtp -qk -qnol -qnodefaultpv /tstdev/OO_procptr/CrossFeatures1/Associated3.f
! opt variations: -qnok -ql -qdefaultpv -qreuse=self

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : May. 9, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure pointer
!*
!*  SECONDARY FUNCTIONS TESTED : Pointer assignment
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
!*  ASSOCIATED(POINTER [, TARGET])
!*   Type binding returns a procptr
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M

    TYPE  :: Base(K1)    ! (4)
      INTEGER, KIND :: K1
      PROCEDURE(INTEGER(8)), POINTER, NOPASS   :: ProcPtr
    END TYPE

    TYPE  :: DT(k2)    ! (4)
      integer, kind  :: k2
      TYPE(Base(k2)) :: BaseComp
      PROCEDURE(INTEGER(8)), POINTER, NOPASS :: ProcPtr
    END TYPE

    CONTAINS

    FUNCTION ModFun()
    INTEGER(8) :: ModFun
      MoDFun = -1
    END FUNCTION

  END MODULE


  PROGRAM Associated3
  USE M
  IMPLICIT NONE
  PROCEDURE(INTEGER(8)), POINTER :: ProcPtr => NULL()
  TYPE (DT(4)) :: V

  V = DT(4)(Base(4)(NULL()), NULL())

  IF ( ASSOCIATED( ProcPtr) )          ERROR STOP 11
  IF ( ASSOCIATED( V%BaseComp%ProcPtr))ERROR STOP 12
  IF ( ASSOCIATED( V%ProcPtr))         ERROR STOP 13

  ProcPtr => ModFun
  V = DT(4)(Base(4)(ModFun), ProcPtr)

  IF ( .NOT. ASSOCIATED( ProcPtr, ModFun) )            ERROR STOP 21
  IF ( .NOT. ASSOCIATED( V%BaseComp%ProcPtr, ModFun) ) ERROR STOP 22
  IF ( .NOT. ASSOCIATED( V%ProcPtr, ModFun))           ERROR STOP 23

  IF (  ProcPtr() .NE. -1 )            ERROR STOP 31
  IF (  V%BaseComp%ProcPtr() .NE. -1 ) ERROR STOP 32
  IF (  V%ProcPtr() .NE. -1 )          ERROR STOP 33


 END

