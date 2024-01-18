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
!*  interaction with type bound generics
!*
!*  -- DTIO/WRITE(UNFORMATTED)
!*  (317038)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M

  TYPE :: DT
    CHARACTER  :: ID
  CONTAINS
    GENERIC    :: WRITE(UNFORMATTED) => WriteUF
    PROCEDURE  :: WriteUF
  END TYPE

  TYPE :: DT1
    CHARACTER  :: ID
  CONTAINS
    GENERIC    :: WRITE(UNFORMATTED) => WriteUF1
    PROCEDURE  :: WriteUF1
  END TYPE

  TYPE :: DT2
    CHARACTER  :: ID
  CONTAINS
    GENERIC    :: WRITE(UNFORMATTED) => WriteUF2
    PROCEDURE  :: WriteUF2
  END TYPE

  TYPE :: DT3
    CHARACTER  :: ID
  CONTAINS
    GENERIC    :: WRITE(UNFORMATTED) => WriteUF3
    PROCEDURE  :: WriteUF3
  END TYPE


  INTERFACE WRITE(UNFORMATTED)
    PROCEDURE WriteUF
    PROCEDURE WriteUF1
    PROCEDURE WriteUF2
    PROCEDURE WriteUF3
  END INTERFACE

  CONTAINS

  SUBROUTINE WriteUF(Dtv, Unit, IOStat, IOMSG)
  CLASS(DT),         INTENT(IN)    :: DTV
  INTEGER,           INTENT(IN)    :: Unit
  INTEGER,           INTENT(OUT)   :: IOSTAT
  CHARACTER (LEN=*), INTENT(INOUT) :: IOMSG
    WRITE(Unit) DTV%ID
  END SUBROUTINE

  SUBROUTINE WriteUF1(Dtv, Unit, IOStat, IOMSG)
  CLASS(DT1),        INTENT(IN)    :: DTV
  INTEGER,           INTENT(IN)    :: Unit
  INTEGER,           INTENT(OUT)   :: IOSTAT
  CHARACTER (LEN=*), INTENT(INOUT) :: IOMSG
    WRITE(Unit) DTV%ID
  END SUBROUTINE

  SUBROUTINE WriteUF2(Dtv, Unit, IOStat, IOMSG)
  CLASS(DT2),        INTENT(IN)    :: DTV
  INTEGER,           INTENT(IN)    :: Unit
  INTEGER,           INTENT(OUT)   :: IOSTAT
  CHARACTER (LEN=*), INTENT(INOUT) :: IOMSG
    WRITE(Unit) DTV%ID
  END SUBROUTINE

  SUBROUTINE WriteUF3(Dtv, Unit, IOStat, IOMSG)
  CLASS(DT3),        INTENT(IN)    :: DTV
  INTEGER,           INTENT(IN)    :: Unit
  INTEGER,           INTENT(OUT)   :: IOSTAT
  CHARACTER (LEN=*), INTENT(INOUT) :: IOMSG
    WRITE(Unit) DTV%ID
  END SUBROUTINE

  END MODULE


  PROGRAM mProcTypBndDTIOWriteU
  USE M


  TYPE(DT)  :: T  = DT ("0")
  TYPE(DT1) :: T1 = DT1("1")
  TYPE(DT2) :: T2 = DT2("2")
  TYPE(DT3) :: T3 = DT3("3")
  CHARACTER :: C, C1, C2, C3


  OPEN(7, STATUS="scratch", FORM="unformatted", ACCESS="sequential",    &
          ACTION="readwrite")

  WRITE(7) T, T1, T2, T3

  REWIND(7)

  READ(7) C, C1, C2, C3

  IF (C   .NE. "0" ) STOP 11
  IF (C1  .NE. "1" ) STOP 12
  IF (C2  .NE. "2" ) STOP 13
  IF (C3  .NE. "3" ) STOP 14

  CLOSE(7)

  END

