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
!*  -- DTIO/WRITE(UNFORMATTED)
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

  INTERFACE WRITE(UNFORMATTED)
    PROCEDURE WriteF
  END INTERFACE

  CONTAINS

  SUBROUTINE WriteF(Dtv, Unit, IOStat, IOMSG)
  CLASS(DT),         INTENT(IN)    :: DTV
  INTEGER,           INTENT(IN)    :: Unit
  INTEGER,           INTENT(OUT)   :: IOSTAT
  CHARACTER (LEN=*), INTENT(INOUT) :: IOMSG
    WRITE(Unit) DTV%ID
  END SUBROUTINE

  SUBROUTINE WriteF1(Dtv, Unit, IOStat, IOMSG)
  CLASS(DT1),        INTENT(IN)    :: DTV
  INTEGER,           INTENT(IN)    :: Unit
  INTEGER,           INTENT(OUT)   :: IOSTAT
  CHARACTER (LEN=*), INTENT(INOUT) :: IOMSG
    WRITE(Unit) DTV%ID
  END SUBROUTINE

  SUBROUTINE WriteF2(Dtv, Unit, IOStat, IOMSG)
  CLASS(DT2),        INTENT(IN)    :: DTV
  INTEGER,           INTENT(IN)    :: Unit
  INTEGER,           INTENT(OUT)   :: IOSTAT
  CHARACTER (LEN=*), INTENT(INOUT) :: IOMSG
    WRITE(Unit) DTV%ID
  END SUBROUTINE


  END MODULE

  SUBROUTINE WriteF3(Dtv, Unit, IOStat, IOMSG)
  USE M
  CLASS(DT3),        INTENT(IN)    :: DTV
  INTEGER,           INTENT(IN)    :: Unit
  INTEGER,           INTENT(OUT)   :: IOSTAT
  CHARACTER (LEN=*), INTENT(INOUT) :: IOMSG
    WRITE(Unit) DTV%ID
  END SUBROUTINE



  PROGRAM mProcDTIOWriteU
  USE M
  USE M1


  INTERFACE WRITE(UNFORMATTED)
    SUBROUTINE WriteF3(Dtv, Unit, IOStat, IOMSG)
      IMPORT
      CLASS(DT3),        INTENT(IN)    :: DTV
      INTEGER,           INTENT(IN)    :: Unit
      INTEGER,           INTENT(OUT)   :: IOSTAT
      CHARACTER (LEN=*), INTENT(INOUT) :: IOMSG
    END SUBROUTINE
  END INTERFACE

  INTERFACE WRITE(UNFORMATTED)
    PROCEDURE WriteF3
  END INTERFACE

  CALL IntSub(WriteF1)

  CONTAINS

  SUBROUTINE IntSub(Proc)
  PROCEDURE(WriteF1)           :: Proc
  PROCEDURE(WriteF2), POINTER  :: ProcPtr

  INTERFACE WRITE(UNFORMATTED)
    PROCEDURE Proc
  END INTERFACE

  INTERFACE WRITE(UNFORMATTED)
    PROCEDURE ProcPtr
  END INTERFACE

  TYPE(DT)  :: T  = DT ("0")
  TYPE(DT1) :: T1 = DT1("1")
  TYPE(DT2) :: T2 = DT2("2")
  TYPE(DT3) :: T3 = DT3("3")
  CHARACTER :: C, C1, C2, C3


  ProcPtr => WriteF2
  OPEN(1, STATUS="scratch", FORM="unformatted", ACCESS="sequential",    &
          ACTION="readwrite")

  WRITE(7) T, T1, T2, T3

  REWIND(7)

  READ(7) C, C1, C2, C3

  IF (C   .NE. "0" ) STOP 11
  IF (C1  .NE. "1" ) STOP 12
  IF (C2  .NE. "2" ) STOP 13
  IF (C3  .NE. "3" ) STOP 14

  END  SUBROUTINE

  END

