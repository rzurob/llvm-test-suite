! GB DTP extension using:
! ftcx_dtp -qck /tstdev/OO_procptr/CrossFeatures2/PtrAssignAssociated1.f
! opt variations: -qnock

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: PtrAssignAssociated1.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : PtrAssignAssociated1.f
!*
!*  DATE                       : Mar. 18, 2005
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
!*  The procedure pointer's status, proc ptr in common
!*  (315172/315269)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE :: DT(K1,N1)    ! (1,4)
    INTEGER, KIND                      :: K1
    INTEGER, LEN                       :: N1
    CHARACTER(kind=K1,len=N1), POINTER :: C
  END TYPE

  CONTAINS

    FUNCTION ModFun(Arg)
    CLASS(*)          :: Arg(:)
    CLASS(*), POINTER :: ModFun(:,:)
    INTEGER           :: I
      I = SQRT(1.*SIZE(Arg, 1))
      ALLOCATE(ModFun(I, I), SOURCE=RESHAPE(Arg, (/I,I/)))
    END FUNCTION

  END MODULE


  PROGRAM PtrAssignAssociated1
  USE M
  IMPLICIT NONE

  PROCEDURE(ModFun), POINTER :: ProcPtr1
  PROCEDURE(ModFun), POINTER :: ProcPtr2
  CHARACTER(4),      POINTER :: CPtr
  COMMON /CB/ProcPtr1, CPtr, ProcPtr2

  ProcPtr1 => ModFun
  CALL IntSub(ProcPtr1)

  CONTAINS

  SUBROUTINE IntSub(ProcPtr)
  PROCEDURE(ProcPtr1), POINTER :: ProcPtr

  INTERFACE
    SUBROUTINE ExtSub(Proc)
      IMPORT
      PROCEDURE(ModFun)          :: Proc
    END SUBROUTINE
  END INTERFACE

  ProcPtr2 => ProcPtr
  ALLOCATE(CPtr, SOURCE="4321")

  CALL ExtSub(ProcPtr)

  END SUBROUTINE

  END

  SUBROUTINE ExtSub(Proc)
  USE M
  IMPLICIT NONE
  PROCEDURE(ModFun)          :: Proc
  PROCEDURE(ModFun), POINTER :: ProcPtr1
  PROCEDURE(ModFun), POINTER :: ProcPtr2
  CHARACTER(4),      POINTER :: CPtr
  INTEGER                    :: i
  COMMON /CB/ProcPtr1, CPtr, ProcPtr2

  IF ( .NOT. ASSOCIATED(CPtr) )  STOP 11
  IF ( CPtr .NE. "4321" )        STOP 12

  IF ( .NOT. ASSOCIATED(ProcPtr1) )            STOP 21
  IF ( .NOT. ASSOCIATED(ProcPtr1, ModFun) )    STOP 22
  IF ( .NOT. ASSOCIATED(ProcPtr1, Proc) )      STOP 23
  IF ( .NOT. ASSOCIATED(ProcPtr1, ProcPtr2) )  STOP 23

  SELECT TYPE ( As => RESHAPE(ProcPtr1((/( CPtr, i=1,256)/)), (/16,16 /) ))
  TYPE IS (CHARACTER(*))
    IF ( ANY( SHAPE(As) .NE. (/16,16/)))    STOP 31
    IF ( ANY( As        .NE. "4321" )  )    STOP 32
  CLASS DEFAULT
    STOP 33
  END SELECT


  END SUBROUTINE

