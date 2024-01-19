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
!*  -- DTIO/Write(FORMATTED)
!*  (314868)
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

  INTERFACE WRITE(FORMATTED)
    PROCEDURE WriteF
  END INTERFACE

  CONTAINS

  SUBROUTINE WriteF(Dtv, Unit, IOTYPE, V_List, IOStat, IOMSG)
  CLASS(DT),         INTENT(IN)    :: DTV
  INTEGER,           INTENT(IN)    :: Unit
  CHARACTER (LEN=*), INTENT(IN)    :: IOTYPE
  INTEGER,           INTENT(IN)    :: V_List(:)
  INTEGER,           INTENT(OUT)   :: IOSTAT
  CHARACTER (LEN=*), INTENT(INOUT) :: IOMSG
    WRITE(Unit, FMT="(A1)") DTV%ID
  END SUBROUTINE

  SUBROUTINE WriteF1(Dtv, Unit, IOTYPE, V_List, IOStat, IOMSG)
  CLASS(DT1),        INTENT(IN)    :: DTV
  INTEGER,           INTENT(IN)    :: Unit
  CHARACTER (LEN=*), INTENT(IN)    :: IOTYPE
  INTEGER,           INTENT(IN)    :: V_List(:)
  INTEGER,           INTENT(OUT)   :: IOSTAT
  CHARACTER (LEN=*), INTENT(INOUT) :: IOMSG
    WRITE(Unit, FMT="(A1)") DTV%ID
  END SUBROUTINE

  SUBROUTINE WriteF2(Dtv, Unit, IOTYPE, V_List, IOStat, IOMSG)
  CLASS(DT2),        INTENT(IN)    :: DTV
  INTEGER,           INTENT(IN)    :: Unit
  CHARACTER (LEN=*), INTENT(IN)    :: IOTYPE
  INTEGER,           INTENT(IN)    :: V_List(:)
  INTEGER,           INTENT(OUT)   :: IOSTAT
  CHARACTER (LEN=*), INTENT(INOUT) :: IOMSG
    WRITE(Unit, FMT="(A1)") DTV%ID
  END SUBROUTINE


  END MODULE

  SUBROUTINE WriteF3(Dtv, Unit, IOTYPE, V_List, IOStat, IOMSG)
  USE M
  CLASS(DT),         INTENT(IN)    :: DTV
  INTEGER,           INTENT(IN)    :: Unit
  CHARACTER (LEN=*), INTENT(IN)    :: IOTYPE
  INTEGER,           INTENT(IN)    :: V_List(:)
  INTEGER,           INTENT(OUT)   :: IOSTAT
  CHARACTER (LEN=*), INTENT(INOUT) :: IOMSG
    WRITE(Unit, FMT="(A1)") DTV%ID
  END SUBROUTINE



  PROGRAM mProcDTIOWriteF
  USE M
  USE M1


  INTERFACE WRITE(FORMATTED)
    SUBROUTINE WriteF3(Dtv, Unit, IOTYPE, V_List, IOStat, IOMSG)
      IMPORT
      CLASS(DT3),        INTENT(IN)    :: DTV
      INTEGER,           INTENT(IN)    :: Unit
      CHARACTER (LEN=*), INTENT(IN)    :: IOTYPE
      INTEGER,           INTENT(IN)    :: V_List(:)
      INTEGER,           INTENT(OUT)   :: IOSTAT
      CHARACTER (LEN=*), INTENT(INOUT) :: IOMSG
    END SUBROUTINE
  END INTERFACE

  INTERFACE WRITE(FORMATTED)
    PROCEDURE WriteF3
  END INTERFACE

  CALL IntSub(WriteF1)

  CONTAINS

  SUBROUTINE IntSub(Proc)
  PROCEDURE(WriteF1)           :: Proc
  PROCEDURE(WriteF2), POINTER  :: ProcPtr

  INTERFACE WRITE(FORMATTED)
    PROCEDURE Proc
  END INTERFACE

  INTERFACE WRITE(FORMATTED)
    PROCEDURE ProcPtr
  END INTERFACE

  TYPE(DT)  :: T  = DT("0")
  TYPE(DT1) :: T1 = DT1("1")
  TYPE(DT2) :: T2 = DT2("2")
  TYPE(DT3) :: T3 = DT3("3")

  CHARACTER(8)  :: Str="????????"

  ProcPtr => WriteF2

  WRITE(Str, *) T, T1, T2, T3
  IF (Str  .NE.  " 0 1 2 3" ) ERROR STOP 11

  END  SUBROUTINE

  END

