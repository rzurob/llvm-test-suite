!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : mProcTypBndDTIOReadF.f
!*
!*  DATE                       : Mar 08, 2006
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
!*  -- DTIO/READ(FORMATTED)
!*  (317038)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M

  TYPE :: DT
    CHARACTER  :: ID
  CONTAINS
    GENERIC    :: READ(FORMATTED) => ReadF
    PROCEDURE  :: ReadF
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

  INTERFACE READ(FORMATTED)
!   PROCEDURE ReadF  ! even though it could be dup here
  END INTERFACE

  CONTAINS

  SUBROUTINE ReadF(Dtv, Unit, IOTYPE, V_List, IOStat, IOMSG)
  CLASS(DT),         INTENT(INOUT) :: DTV
  INTEGER,           INTENT(IN)    :: Unit
  CHARACTER (LEN=*), INTENT(IN)    :: IOTYPE
  INTEGER,           INTENT(IN)    :: V_List(:)
  INTEGER,           INTENT(OUT)   :: IOSTAT
  CHARACTER (LEN=*), INTENT(INOUT) :: IOMSG
    READ(Unit, FMT="(A1)") DTV%ID
  END SUBROUTINE

  SUBROUTINE ReadF1(Dtv, Unit, IOTYPE, V_List, IOStat, IOMSG)
  CLASS(DT1),        INTENT(INOUT) :: DTV
  INTEGER,           INTENT(IN)    :: Unit
  CHARACTER (LEN=*), INTENT(IN)    :: IOTYPE
  INTEGER,           INTENT(IN)    :: V_List(:)
  INTEGER,           INTENT(OUT)   :: IOSTAT
  CHARACTER (LEN=*), INTENT(INOUT) :: IOMSG
    READ(Unit, FMT="(A1)") DTV%ID
  END SUBROUTINE

  SUBROUTINE ReadF2(Dtv, Unit, IOTYPE, V_List, IOStat, IOMSG)
  CLASS(DT2),        INTENT(INOUT) :: DTV
  INTEGER,           INTENT(IN)    :: Unit
  CHARACTER (LEN=*), INTENT(IN)    :: IOTYPE
  INTEGER,           INTENT(IN)    :: V_List(:)
  INTEGER,           INTENT(OUT)   :: IOSTAT
  CHARACTER (LEN=*), INTENT(INOUT) :: IOMSG
    READ(Unit, FMT="(A1)") DTV%ID
  END SUBROUTINE


  END MODULE

  SUBROUTINE ReadF3(Dtv, Unit, IOTYPE, V_List, IOStat, IOMSG)
  USE M
  CLASS(DT3),        INTENT(INOUT) :: DTV
  INTEGER,           INTENT(IN)    :: Unit
  CHARACTER (LEN=*), INTENT(IN)    :: IOTYPE
  INTEGER,           INTENT(IN)    :: V_List(:)
  INTEGER,           INTENT(OUT)   :: IOSTAT
  CHARACTER (LEN=*), INTENT(INOUT) :: IOMSG
    READ(Unit, FMT="(A1)") DTV%ID
  END SUBROUTINE



  PROGRAM mProcTypBndDTIOReadF
  USE M

  INTERFACE READ(FORMATTED)
    SUBROUTINE ReadF3(Dtv, Unit, IOTYPE, V_List, IOStat, IOMSG)
      IMPORT
      CLASS(DT3),        INTENT(INOUT) :: DTV
      INTEGER,           INTENT(IN)    :: Unit
      CHARACTER (LEN=*), INTENT(IN)    :: IOTYPE
      INTEGER,           INTENT(IN)    :: V_List(:)
      INTEGER,           INTENT(OUT)   :: IOSTAT
      CHARACTER (LEN=*), INTENT(INOUT) :: IOMSG
    END SUBROUTINE
  END INTERFACE

  INTERFACE READ(FORMATTED)
    PROCEDURE ReadF3
  END INTERFACE

  CALL IntSub(ReadF1)

  CONTAINS

  SUBROUTINE IntSub(Proc)
  PROCEDURE(ReadF1)           :: Proc
  PROCEDURE(ReadF2), POINTER  :: ProcPtr

  INTERFACE READ(FORMATTED)
    PROCEDURE Proc
  END INTERFACE

  INTERFACE READ(FORMATTED)
    PROCEDURE ProcPtr
  END INTERFACE

  TYPE(DT)  :: T
  TYPE(DT1) :: T1
  TYPE(DT2) :: T2
  TYPE(DT3) :: T3
  CHARACTER(4)  :: Str="0123"

  ProcPtr => ReadF2

  READ(Str, *) T, T1, T2, T3

  IF (T%ID  .NE.  "0" ) STOP 11
  IF (T1%ID  .NE. "1" ) STOP 12
  IF (T2%ID  .NE. "2" ) STOP 13
  IF (T3%ID  .NE. "3" ) STOP 14

  END  SUBROUTINE

  END

