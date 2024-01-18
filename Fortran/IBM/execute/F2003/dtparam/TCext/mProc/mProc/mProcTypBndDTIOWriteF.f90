! GB DTP extension using:
! ftcx_dtp -qk /tstdev/F2003/mProc/mProc/mProcTypBndDTIOWriteF.f
! opt variations: -qck -qnok

!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Mar 07, 2006
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
!*  -- DTIO/Write(FORMATTED)
!*  (316777/317038)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M

  TYPE :: DT(K1,N1)    ! (4,1)
    INTEGER, KIND :: K1
    INTEGER, LEN  :: N1
    CHARACTER(N1) :: ID
  CONTAINS
    GENERIC    :: WRITE(FORMATTED) => WriteF
    PROCEDURE  :: WriteF
  END TYPE

  TYPE :: DT1(K2,N2)    ! (4,1)
    INTEGER, KIND :: K2
    INTEGER, LEN  :: N2
    CHARACTER(N2) :: ID
  END TYPE

  TYPE :: DT2(K3,N3)    ! (4,1)
    INTEGER, KIND :: K3
    INTEGER, LEN  :: N3
    CHARACTER(N3) :: ID
  END TYPE

  TYPE :: DT3(K4,N4)    ! (4,1)
    INTEGER, KIND :: K4
    INTEGER, LEN  :: N4
    CHARACTER(N4) :: ID
  END TYPE

  INTERFACE WRITE(FORMATTED)
    MODULE PROCEDURE WriteF
  END INTERFACE

  CONTAINS

  SUBROUTINE WriteF(Dtv, Unit, IOTYPE, V_List, IOStat, IOMSG)
  CLASS(DT(4,*)),         INTENT(IN)    :: DTV
  INTEGER,           INTENT(IN)    :: Unit
  CHARACTER (LEN=*), INTENT(IN)    :: IOTYPE
  INTEGER,           INTENT(IN)    :: V_List(:)
  INTEGER,           INTENT(OUT)   :: IOSTAT
  CHARACTER (LEN=*), INTENT(INOUT) :: IOMSG
    WRITE(Unit, FMT="(A1)") DTV%ID
  END SUBROUTINE

  SUBROUTINE WriteF1(Dtv, Unit, IOTYPE, V_List, IOStat, IOMSG)
  CLASS(DT1(4,*)),        INTENT(IN)    :: DTV
  INTEGER,           INTENT(IN)    :: Unit
  CHARACTER (LEN=*), INTENT(IN)    :: IOTYPE
  INTEGER,           INTENT(IN)    :: V_List(:)
  INTEGER,           INTENT(OUT)   :: IOSTAT
  CHARACTER (LEN=*), INTENT(INOUT) :: IOMSG
    WRITE(Unit, FMT="(A1)") DTV%ID
  END SUBROUTINE

  SUBROUTINE WriteF2(Dtv, Unit, IOTYPE, V_List, IOStat, IOMSG)
  CLASS(DT2(4,*)),        INTENT(IN)    :: DTV
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
  CLASS(DT(4,*)),         INTENT(IN)    :: DTV
  INTEGER,           INTENT(IN)    :: Unit
  CHARACTER (LEN=*), INTENT(IN)    :: IOTYPE
  INTEGER,           INTENT(IN)    :: V_List(:)
  INTEGER,           INTENT(OUT)   :: IOSTAT
  CHARACTER (LEN=*), INTENT(INOUT) :: IOMSG
    WRITE(Unit, FMT="(A1)") DTV%ID
  END SUBROUTINE



  PROGRAM  mProcTypBndDTIOWriteF
  USE M

  INTERFACE
    SUBROUTINE WriteF3(Dtv, Unit, IOTYPE, V_List, IOStat, IOMSG)
      IMPORT
      CLASS(DT3(4,*)),        INTENT(IN)    :: DTV
      INTEGER,           INTENT(IN)    :: Unit
      CHARACTER (LEN=*), INTENT(IN)    :: IOTYPE
      INTEGER,           INTENT(IN)    :: V_List(:)
      INTEGER,           INTENT(OUT)   :: IOSTAT
      CHARACTER (LEN=*), INTENT(INOUT) :: IOMSG
    END SUBROUTINE
  END INTERFACE

  INTERFACE WRITE(FORMATTED)
    PROCEDURE ProcPtr1
  END INTERFACE

  PROCEDURE(WriteF2), POINTER  :: ProcPtr
  PROCEDURE(WriteF3), POINTER  :: ProcPtr1

  ProcPtr => WriteF2
  ProcPtr1 => WriteF3
  CALL IntSub(WriteF1, ProcPTr)

  CONTAINS

  SUBROUTINE IntSub(Proc, ProcPtr)
  PROCEDURE(WriteF1)           :: Proc
  PROCEDURE(WriteF2), POINTER  :: ProcPtr

  INTERFACE WRITE(FORMATTED)
    PROCEDURE Proc
  END INTERFACE

  INTERFACE WRITE(FORMATTED)
    PROCEDURE ProcPtr
  END INTERFACE

  TYPE(DT(4,1))  :: T  = DT(4,1)("0")
  TYPE(DT1(4,1)) :: T1 = DT1(4,1)("1")
  TYPE(DT2(4,1)) :: T2 = DT2(4,1)("2")
  TYPE(DT3(4,1)) :: T3 = DT3(4,1)("3")

  CHARACTER(9)  :: Str="?????????"

  WRITE(Str, *) T, T1, T2, T3
  IF (Str  .NE.  " 0 1 2 3 " ) ERROR STOP 11

  END  SUBROUTINE

  END

