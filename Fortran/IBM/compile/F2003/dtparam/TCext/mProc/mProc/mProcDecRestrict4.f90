! GB DTP extension using:
! ftcx_dtp -qk /tstdev/F2003/mProc/mProc/mProcDecRestrict4.f
! opt variations: -qck -qnok

!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Mar. 10, 2006
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
!*  Within a scoping unit, if two procedures have the same dtio-generic-spec (12.3.2.1),
!*  their dtv arguments shall be type incompatible or have different kind type parameters.
!*
!*  (317262)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  PROCEDURE(ReadF1), POINTER :: ProcPtr

  TYPE :: DT(K1,N1)    ! (4,1)
    INTEGER, KIND :: K1
    INTEGER, LEN  :: N1
    CHARACTER(N1) :: ID
  CONTAINS
    GENERIC    :: READ(FORMATTED) => ReadF
    PROCEDURE  :: ReadF
  END TYPE

  TYPE, EXTENDS(DT) :: DT1    ! (4,1)
  END TYPE

  CONTAINS

  SUBROUTINE ReadF(Dtv, Unit, IOTYPE, V_List, IOStat, IOMSG)
  CLASS(DT(4,*)),         INTENT(INOUT) :: DTV
  INTEGER,           INTENT(IN)    :: Unit
  CHARACTER (LEN=*), INTENT(IN)    :: IOTYPE
  INTEGER,           INTENT(IN)    :: V_List(:)
  INTEGER,           INTENT(OUT)   :: IOSTAT
  CHARACTER (LEN=*), INTENT(INOUT) :: IOMSG
    READ(Unit, FMT="(A1)") DTV%ID
  END SUBROUTINE

  SUBROUTINE ReadF1(Dtv, Unit, IOTYPE, V_List, IOStat, IOMSG)
  CLASS(DT1(4,*)),        INTENT(INOUT) :: DTV
  INTEGER,           INTENT(IN)    :: Unit
  CHARACTER (LEN=*), INTENT(IN)    :: IOTYPE
  INTEGER,           INTENT(IN)    :: V_List(:)
  INTEGER,           INTENT(OUT)   :: IOSTAT
  CHARACTER (LEN=*), INTENT(INOUT) :: IOMSG
    READ(Unit, FMT="(A1)") DTV%ID
  END SUBROUTINE


  END MODULE

  PROGRAM mProcDecRestrict4
  USE M

  CONTAINS

  SUBROUTINE IntSub(Proc)
  PROCEDURE(ReadF1) :: Proc

  INTERFACE READ(FORMATTED)
    PROCEDURE ProcPtr
  END INTERFACE

  INTERFACE READ(FORMATTED)
    PROCEDURE Proc
  END INTERFACE

  ! the following is ok
  INTERFACE READ(FORMATTED)
    PROCEDURE ReadF
  END INTERFACE

  END SUBROUTINE

  END


