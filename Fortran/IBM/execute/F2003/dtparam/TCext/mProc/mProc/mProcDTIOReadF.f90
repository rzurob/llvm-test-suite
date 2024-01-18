! GB DTP extension using:
! ftcx_dtp -qk /tstdev/F2003/mProc/mProc/mProcDTIOReadF.f
! opt variations: -qck -qnok

!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : mProcDTIOReadF.f  
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Mar 01, 2006
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
!*  A generic interface block specifies a generic interface for each of the
!*  procedures in the interface block. The PROCEDURE statement lists procedure 
!*  pointers, external procedures, du mmy procedures, or module procedures
!*  that have this generic interface. A generic interface is always explicit.
!*  -- DTIO/READ(FORMATTED) 
!*  (316848)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M

  TYPE :: DT(K1,N1)    ! (4,1)
    INTEGER, KIND :: K1
    INTEGER, LEN  :: N1
    CHARACTER(N1) :: ID
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

  END MODULE

  MODULE M1
  USE M
 
  INTERFACE READ(FORMATTED) 
    PROCEDURE ReadF 
  END INTERFACE  

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

  SUBROUTINE ReadF2(Dtv, Unit, IOTYPE, V_List, IOStat, IOMSG)
  CLASS(DT2(4,*)),        INTENT(INOUT) :: DTV 
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
  CLASS(DT(4,*)),         INTENT(INOUT) :: DTV 
  INTEGER,           INTENT(IN)    :: Unit 
  CHARACTER (LEN=*), INTENT(IN)    :: IOTYPE 
  INTEGER,           INTENT(IN)    :: V_List(:)
  INTEGER,           INTENT(OUT)   :: IOSTAT 
  CHARACTER (LEN=*), INTENT(INOUT) :: IOMSG 
    READ(Unit, FMT="(A1)") DTV%ID  
  END SUBROUTINE 



  PROGRAM mProcDTIOReadF 
  USE M
  USE M1


  INTERFACE READ(FORMATTED) 
    SUBROUTINE ReadF3(Dtv, Unit, IOTYPE, V_List, IOStat, IOMSG)
      IMPORT
      CLASS(DT3(4,*)),        INTENT(INOUT) :: DTV
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

  TYPE(DT(4,1))  :: T
  TYPE(DT1(4,1)) :: T1
  TYPE(DT2(4,1)) :: T2
  TYPE(DT3(4,1)) :: T3
  CHARACTER(7)  :: Str="0 1 2 3"

  ProcPtr => ReadF2

  READ(Str, *) T, T1, T2, T3

  IF (T%ID  .NE.  "0" ) STOP 11
  IF (T1%ID  .NE. "1" ) STOP 12
  IF (T2%ID  .NE. "2" ) STOP 13
  IF (T3%ID  .NE. "3" ) STOP 14

  END  SUBROUTINE

  END

