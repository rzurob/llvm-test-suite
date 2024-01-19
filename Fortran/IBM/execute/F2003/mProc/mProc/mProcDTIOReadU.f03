!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Mar 02, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Generaliztion of PROCEDURE statement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 296676
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  A generic interface block specifies a generic interface for each of the
!*  procedures in the interface block. The PROCEDURE statement lists procedure
!*  pointers, external procedures, du mmy procedures, or module procedures
!*  that have this generic interface. A generic interface is always explicit.
!*  -- DTIO/READ(UNFORMATTED)
!*  (316848)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M

  TYPE :: DT
    CHARACTER :: ID
  END TYPE

  TYPE :: DT1
    CHARACTER :: ID
  END TYPE

  TYPE :: DT2
    CHARACTER :: ID
  END TYPE

  TYPE :: DT3
    CHARACTER :: ID
  END TYPE

  END MODULE

  MODULE M1
  USE M

  INTERFACE READ(UNFORMATTED)
    PROCEDURE ReadF
  END INTERFACE

  CONTAINS

  SUBROUTINE ReadF(Dtv, Unit, IOStat, IOMSG)
  CLASS(DT),         INTENT(INOUT) :: DTV
  INTEGER,           INTENT(IN)    :: Unit
  INTEGER,           INTENT(OUT)   :: IOSTAT
  CHARACTER (LEN=*), INTENT(INOUT) :: IOMSG
    READ(Unit) DTV%ID
  END SUBROUTINE

  SUBROUTINE ReadF1(Dtv, Unit, IOStat, IOMSG)
  CLASS(DT1),        INTENT(INOUT) :: DTV
  INTEGER,           INTENT(IN)    :: Unit
  INTEGER,           INTENT(OUT)   :: IOSTAT
  CHARACTER (LEN=*), INTENT(INOUT) :: IOMSG
    READ(Unit) DTV%ID
  END SUBROUTINE

  SUBROUTINE ReadF2(Dtv, Unit, IOStat, IOMSG)
  CLASS(DT2),        INTENT(INOUT) :: DTV
  INTEGER,           INTENT(IN)    :: Unit
  INTEGER,           INTENT(OUT)   :: IOSTAT
  CHARACTER (LEN=*), INTENT(INOUT) :: IOMSG
    READ(Unit) DTV%ID
  END SUBROUTINE


  END MODULE

  SUBROUTINE ReadF3(Dtv, Unit, IOStat, IOMSG)
  USE M
  CLASS(DT),         INTENT(INOUT) :: DTV
  INTEGER,           INTENT(IN)    :: Unit
  INTEGER,           INTENT(OUT)   :: IOSTAT
  CHARACTER (LEN=*), INTENT(INOUT) :: IOMSG
    READ(Unit) DTV%ID
  END SUBROUTINE



  PROGRAM mProcDTIOReadU
  USE M
  USE M1


  INTERFACE READ(UNFORMATTED)
    SUBROUTINE ReadF3(Dtv, Unit, IOStat, IOMSG)
      IMPORT
      CLASS(DT3),        INTENT(INOUT) :: DTV
      INTEGER,           INTENT(IN)    :: Unit
      INTEGER,           INTENT(OUT)   :: IOSTAT
      CHARACTER (LEN=*), INTENT(INOUT) :: IOMSG
    END SUBROUTINE
  END INTERFACE

  INTERFACE READ(UNFORMATTED)
    PROCEDURE ReadF3
  END INTERFACE

  CALL IntSub(ReadF1)

  CONTAINS

  SUBROUTINE IntSub(Proc)
  PROCEDURE(ReadF1)           :: Proc
  PROCEDURE(ReadF2), POINTER  :: ProcPtr

  INTERFACE READ(UNFORMATTED)
    PROCEDURE Proc
  END INTERFACE

  INTERFACE READ(UNFORMATTED)
    PROCEDURE ProcPtr
  END INTERFACE

  TYPE(DT)  :: T
  TYPE(DT1) :: T1
  TYPE(DT2) :: T2
  TYPE(DT3) :: T3

  ProcPtr => ReadF2

  OPEN(1, STATUS="scratch", FORM="unformatted", ACCESS="sequential",    &
          ACTION="readwrite")

  WRITE(7) "0", "1", "2", "3"
  REWIND(7)

  READ(7) T, T1, T2, T3

  IF (T%ID   .NE. "0" ) ERROR STOP 11
  IF (T1%ID  .NE. "1" ) ERROR STOP 12
  IF (T2%ID  .NE. "2" ) ERROR STOP 13
  IF (T3%ID  .NE. "3" ) ERROR STOP 14

  END  SUBROUTINE

  END

