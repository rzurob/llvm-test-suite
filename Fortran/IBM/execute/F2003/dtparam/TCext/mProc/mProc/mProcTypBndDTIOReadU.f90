! GB DTP extension using:
! ftcx_dtp -qk /tstdev/F2003/mProc/mProc/mProcTypBndDTIOReadU.f
! opt variations: -qck -qnok

!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : mProcTypBndDTIOReadU.f  
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Mar 08, 2006
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Generaliztion of PROCEDURE statement 
!*
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  REFERENCE                  : Feature Number 296676 
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  
!*  Interaction with type bound generics 
!*  
!*  -- DTIO/READ(UNFORMATTED) 
!*  (317038)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M

  TYPE :: DT(K1,N1)    ! (4,1)
    INTEGER, KIND :: K1
    INTEGER, LEN  :: N1
    CHARACTER(N1) :: ID
  CONTAINS
    GENERIC    :: READ(UNFORMATTED) => ReadUF
    PROCEDURE  :: ReadUF
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
  CONTAINS
    GENERIC    :: READ(UNFORMATTED) => ReadUF3
    PROCEDURE  :: ReadUF3
  END TYPE

  INTERFACE READ(UNFORMATTED) 
 !  PROCEDURE ReadUF 
  END INTERFACE  

  INTERFACE READ(UNFORMATTED) 
    PROCEDURE ReadUF3 
  END INTERFACE  

  CONTAINS

  SUBROUTINE ReadUF(Dtv, Unit, IOStat, IOMSG)
  CLASS(DT(4,*)),         INTENT(INOUT) :: DTV 
  INTEGER,           INTENT(IN)    :: Unit 
  INTEGER,           INTENT(OUT)   :: IOSTAT 
  CHARACTER (LEN=*), INTENT(INOUT) :: IOMSG 
    READ(Unit) DTV%ID  
  END SUBROUTINE 

  SUBROUTINE ReadUF1(Dtv, Unit, IOStat, IOMSG)
  CLASS(DT1(4,*)),        INTENT(INOUT) :: DTV 
  INTEGER,           INTENT(IN)    :: Unit 
  INTEGER,           INTENT(OUT)   :: IOSTAT 
  CHARACTER (LEN=*), INTENT(INOUT) :: IOMSG 
    READ(Unit) DTV%ID  
  END SUBROUTINE 

  SUBROUTINE ReadUF2(Dtv, Unit, IOStat, IOMSG)
  CLASS(DT2(4,*)),        INTENT(INOUT) :: DTV 
  INTEGER,           INTENT(IN)    :: Unit 
  INTEGER,           INTENT(OUT)   :: IOSTAT 
  CHARACTER (LEN=*), INTENT(INOUT) :: IOMSG 
    READ(Unit) DTV%ID  
  END SUBROUTINE 

  SUBROUTINE ReadUF3(Dtv, Unit, IOStat, IOMSG)
  CLASS(DT3(4,*)),        INTENT(INOUT) :: DTV 
  INTEGER,           INTENT(IN)    :: Unit 
  INTEGER,           INTENT(OUT)   :: IOSTAT 
  CHARACTER (LEN=*), INTENT(INOUT) :: IOMSG 
    READ(Unit) DTV%ID  
  END SUBROUTINE 

  END MODULE


  PROGRAM mProcTypBndDTIOReadU 
  USE M

  CALL IntSub(ReadUF1)

  CONTAINS

  SUBROUTINE IntSub(Proc)
  PROCEDURE(ReadUF1)           :: Proc
  PROCEDURE(ReadUF2), POINTER  :: ProcPtr

  INTERFACE READ(UNFORMATTED)
    PROCEDURE Proc 
  END INTERFACE

  INTERFACE READ(UNFORMATTED) 
    PROCEDURE ProcPtr 
  END INTERFACE

  TYPE(DT(4,1))  :: T
  TYPE(DT1(4,1)) :: T1
  TYPE(DT2(4,1)) :: T2
  TYPE(DT3(4,1)) :: T3

  ProcPtr => ReadUF2

  OPEN(1, STATUS="scratch", FORM="unformatted", ACCESS="sequential",    &
          ACTION="readwrite")

  WRITE(7) "0", "1", "2", "3"
  REWIND(7)

  READ(7) T, T1, T2, T3

  IF (T%ID   .NE. "0" ) STOP 11
  IF (T1%ID  .NE. "1" ) STOP 12
  IF (T2%ID  .NE. "2" ) STOP 13
  IF (T3%ID  .NE. "3" ) STOP 14

  END  SUBROUTINE

  END

