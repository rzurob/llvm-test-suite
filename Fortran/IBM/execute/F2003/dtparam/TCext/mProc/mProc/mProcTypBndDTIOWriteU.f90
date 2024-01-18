! GB DTP extension using:
! ftcx_dtp -qck /tstdev/F2003/mProc/mProc/mProcTypBndDTIOWriteU.f
! opt variations: -qnock

!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : mProcTypBndDTIOWriteU.f  
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Mar 02, 2006
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
!*  interaction with type bound generics 
!*  
!*
!*  -- DTIO/WRITE(UNFORMATTED) 
!*  (317038)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M

  TYPE :: DT(K1,N1)    ! (1,1)
    INTEGER, KIND             :: K1
    INTEGER, LEN              :: N1
    CHARACTER(kind=K1,len=N1) :: ID
  CONTAINS
    GENERIC    :: WRITE(UNFORMATTED) => WriteUF
    PROCEDURE  :: WriteUF 
  END TYPE
 
  TYPE :: DT1(K2,N2)    ! (1,1)
    INTEGER, KIND             :: K2
    INTEGER, LEN              :: N2
    CHARACTER(kind=K2,len=N2) :: ID
  CONTAINS
    GENERIC    :: WRITE(UNFORMATTED) => WriteUF1
    PROCEDURE  :: WriteUF1 
  END TYPE
 
  TYPE :: DT2(K3,N3)    ! (1,1)
    INTEGER, KIND             :: K3
    INTEGER, LEN              :: N3
    CHARACTER(kind=K3,len=N3) :: ID
  CONTAINS
    GENERIC    :: WRITE(UNFORMATTED) => WriteUF2
    PROCEDURE  :: WriteUF2 
  END TYPE
 
  TYPE :: DT3(K4,N4)    ! (1,1)
    INTEGER, KIND             :: K4
    INTEGER, LEN              :: N4
    CHARACTER(kind=K4,len=N4) :: ID
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
  CLASS(DT(1,*)),         INTENT(IN)    :: DTV 
  INTEGER,           INTENT(IN)    :: Unit 
  INTEGER,           INTENT(OUT)   :: IOSTAT 
  CHARACTER (LEN=*), INTENT(INOUT) :: IOMSG 
    WRITE(Unit) DTV%ID  
  END SUBROUTINE 

  SUBROUTINE WriteUF1(Dtv, Unit, IOStat, IOMSG)
  CLASS(DT1(1,*)),        INTENT(IN)    :: DTV 
  INTEGER,           INTENT(IN)    :: Unit 
  INTEGER,           INTENT(OUT)   :: IOSTAT 
  CHARACTER (LEN=*), INTENT(INOUT) :: IOMSG 
    WRITE(Unit) DTV%ID  
  END SUBROUTINE 

  SUBROUTINE WriteUF2(Dtv, Unit, IOStat, IOMSG)
  CLASS(DT2(1,*)),        INTENT(IN)    :: DTV 
  INTEGER,           INTENT(IN)    :: Unit 
  INTEGER,           INTENT(OUT)   :: IOSTAT 
  CHARACTER (LEN=*), INTENT(INOUT) :: IOMSG 
    WRITE(Unit) DTV%ID  
  END SUBROUTINE 

  SUBROUTINE WriteUF3(Dtv, Unit, IOStat, IOMSG)
  CLASS(DT3(1,*)),        INTENT(IN)    :: DTV 
  INTEGER,           INTENT(IN)    :: Unit 
  INTEGER,           INTENT(OUT)   :: IOSTAT 
  CHARACTER (LEN=*), INTENT(INOUT) :: IOMSG 
    WRITE(Unit) DTV%ID  
  END SUBROUTINE 

  END MODULE


  PROGRAM mProcTypBndDTIOWriteU 
  USE M


  TYPE(DT(1,1))  :: T  = DT(1,1) ("0")
  TYPE(DT1(1,1)) :: T1 = DT1(1,1)("1")
  TYPE(DT2(1,1)) :: T2 = DT2(1,1)("2")
  TYPE(DT3(1,1)) :: T3 = DT3(1,1)("3")
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

