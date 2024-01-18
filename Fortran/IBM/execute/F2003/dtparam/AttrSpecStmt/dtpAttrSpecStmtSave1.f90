!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtpAttrSpecStmtSave1
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jun. 13, 2007
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : DERIVED TYPE PARAMETERS
!*
!*  SECONDARY FUNCTIONS TESTED : Data Object Declaration 
!*
!*  REFERENCE                  : Feature Number 289057
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
!* 
!*  -- SAVE statement 
!*  
!*   common block 
!* 
!* (337927)
!*   
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M


  TYPE :: DT0(K0, L0)
    INTEGER, KIND :: K0=1
    INTEGER, LEN  :: L0=1
    SEQUENCE
  END TYPE

  TYPE  :: DT1(K1, L1)
    INTEGER, KIND    :: K1=0
    INTEGER, LEN     :: L1=0
    SEQUENCE
    CHARACTER(L1+3)      :: C1 = "DT1" 
  END TYPE

  TYPE :: DT2(K2,L2)
    INTEGER, KIND    :: K2
    INTEGER, LEN     :: L2
    SEQUENCE
    CHARACTER(L2)        :: C2=CHAR(K2)
    INTEGER(K2)          :: I=K2
    REAL   (K2)          :: R=K2
    LOGICAL(K2)          :: L=.TRUE._1
    COMPLEX(K2)          :: Z=CMPLX(K2, K2, K2)
    TYPE(DT0(K2, L2))    :: T0(L2) 
    TYPE(DT2(K2, L2)), POINTER  :: Ptr 
  END TYPE

  INTEGER,   PARAMETER   :: N=1024

  TYPE(DT0(1,3)), save :: T0
  TYPE(DT1(4,5)), save :: T1
  TYPE(Dt2(8,7)), save :: T2
  DIMENSION      :: T0(N), T2(N)
  DIMENSION      :: T1(N)

  SAVE  MyBlock 
  COMMON /MyBlock/T0,T1,T2


  TYPE(DT2(8,7)), PARAMETER ::                     &
        CT =  DT2(8,7)   (                         &
                                  C2 = "ZYX",      &
                                   I = 1234,       &
                                   R = 4321.,      &
                                   L = .TRUE.,     &
                                   Z = (1.,-1.),   &
                                   Ptr  = NULL(),  &
                                   T0=DT0(8,7)() ) 


  END MODULE

  SUBROUTINE ExtSub()
  USE M


  DO I=1, N 

    IF ( T0(I)%L0     .NE. 3         )  STOP 30
    IF ( T1(I)%L1     .NE. 5         )  STOP 32
    IF ( T2(I)%t0%L0     .NE. 7         )  STOP 33
    IF ( T2(I)%L2     .NE. 7         )  STOP 35

    IF ( T1(I)%C1     .NE. "ZYX"     )  STOP 40

    IF ( T2(I)%C2             .NE.   "ZYX"    )  STOP 52
    IF ( T2(I)%I              .NE.   1234     )  STOP 53
    IF ( T2(I)%R              .NE.   4321.    )  STOP 54
    IF ( T2(I)%L              .NEQV. .TRUE.   )  STOP 55
    IF ( T2(I)%Z              .NE.   (1.,-1.) )  STOP 56
    IF ( T2(I)%T0%K0          .NE.    8       )  STOP 57
    IF ( T2(I)%T0%L0          .NE.    7       )  STOP 61
    IF ( ASSOCIATED(T2(I)%Ptr).EQV.   .TRUE.  )  STOP 62
    IF ( SIZE(T2(I)%T0)       .NE.    7       )  STOP 63
    IF ( T2(I)%T0%K0          .NE.    8       )  STOP 64
    IF ( T2(I)%T0%L0          .NE.    7       )  STOP 65
  END DO

  END SUBROUTINE


  PROGRAM dtpAttrSpecStmtSave1
  USE M
  IMPLICIT NONE
  INTEGER I
 
  INTERFACE 
    SUBROUTINE ExtSub()
    END SUBROUTINE
  END INTERFACE


  T1%c1 = CT%c2
  T2 = CT

  CALL ExtSub()

  END


