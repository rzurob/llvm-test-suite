! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Dec. 13, 2004
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
!*   The selector is of logical
!*    (Comp failed)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM SltLogical
  IMPLICIT NONE

  BYTE              :: B=1_1
  CLASS(*), POINTER :: Log(:)

  ALLOCATE(Log(4), SOURCE=.TRUE._1)

  SELECT TYPE ( As => Log )
  CLASS DEFAULT
    STOP 30
  TYPE IS ( LOGICAL(1) )
    IF ( ANY(SHAPE(As) .NE. (/4/))  )   STOP 31
    IF ( LBOUND(As,1)  .NE. 1       )   STOP 32
    IF ( ANY(As        .NEQV. (/.TRUE.,.TRUE.,.TRUE.,.TRUE./)) )  STOP 32
  TYPE IS (LOGICAL )
    STOP 34
  END SELECT

  DEALLOCATE(Log)
  ALLOCATE(Log(4), SOURCE=.TRUE._2)

  SELECT TYPE ( Log )
  CLASS DEFAULT
    STOP 40
  TYPE IS (LOGICAL(1)  )
    STOP 41
  TYPE IS ( LOGICAL(2) )
    IF ( ANY(SHAPE(Log) .NE. (/4/))  )   STOP 42
    IF ( LBOUND(Log, 1) .NE. 1       )   STOP 43
    IF ( ANY(Log        .NEQV. (/.TRUE.,.TRUE.,.TRUE.,.TRUE./)) )  STOP 44
  TYPE IS (LOGICAL )
    STOP 45
  END SELECT

  DEALLOCATE(Log)
  ALLOCATE(Log(4), SOURCE=.FALSE._8)

  SELECT TYPE ( As => Log(::3) )
  CLASS DEFAULT
    STOP 50
  TYPE IS ( LOGICAL(  8) )
    As = .TRUE.
    IF ( ANY(SHAPE(As) .NE. (/2/))  )   STOP 51
    IF ( LBOUND(As, 1) .NE. 1       )   STOP 52
    IF ( ANY(As        .NEQV. (/.TRUE.,.TRUE./)) )  STOP 52
  TYPE IS (CHARACTER(*))
    STOP 54
  END SELECT

  DEALLOCATE(Log)
  ALLOCATE(Log(4), SOURCE=B)

  SELECT TYPE ( As => Log(3) )
  CLASS DEFAULT
    STOP 60
  TYPE IS (     BYTE)
    IF ( As   .NE. 1_1 )  STOP 62
  TYPE IS (LOGICAL(4)     )
    STOP 64
  END SELECT

  DEALLOCATE(Log)

  END

