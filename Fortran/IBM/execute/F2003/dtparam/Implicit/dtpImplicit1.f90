!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtpImplicit1
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jun. 22, 2007
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
!*  -- The implicit statement
!*  tests its scope 
!* 
!*  (ICE)
!*   
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  IMPLICIT TYPE(DT0(1,3))(R)

  TYPE :: DT0(K0, L0)
    INTEGER, KIND :: K0=1
    INTEGER, LEN  :: L0=1
  END TYPE

  TYPE,  EXTENDS(DT0)  :: DT1(K1, L1)
    INTEGER(K0), KIND :: K1=1
    INTEGER(K0), LEN  :: L1=1
    REAL(K1) :: R(L1)=K1
  END TYPE

  TYPE, EXTENDS(DT1) :: DT2(K2,L2)
    INTEGER(K1), KIND :: K2=1
    INTEGER(K1), LEN  :: L2=1
    CHARACTER(L2) :: C(L2)=CHAR(K2)
    INTEGER(K2)   :: I(L2)=K2
    TYPE(DT2(K0,L0,K1,L0,K2,L2)), POINTER :: Ptr=>NULL()
  END TYPE

  POINTER :: RP

  INTERFACE 
    SUBROUTINE ExtSub(R)
    END SUBROUTINE
  END INTERFACE

  END MODULE

  SUBROUTINE ExtSub(R)
    R = -R
  END SUBROUTINE


  PROGRAM dtpImplicit1
  USE M
  IMPLICIT TYPE(DT1(1,3,4,5))(S)

  TYPE(DT0(1,3)), TARGET :: RT  

  RP => RT
  IF ( .NOT. ASSOCIATED(RP) ) STOP 71
  RR = -1
  IF ( RR .NE. -1 ) STOP 72
  
  CALL ExtSub(RR)
  IF ( RR .NE. 1 ) STOP 73

  TT = -1._16
  IF ( TT .NE. -1 ) STOP 74

  CALL IntSub()

  CONTAINS

  SUBROUTINE IntSub()
  IMPLICIT TYPE(DT2(1,3,4,5,8,7))(T)

  IF ( SIZE( S%R )      .NE. S%L1            ) STOP 12
  IF ( S%R%KIND         .NE. S%K1            ) STOP 13
  IF ( ANY ( S%R        .NE. S%K1          ) ) STOP 14

  IF ( SIZE( T%R )      .NE. T%L1         ) STOP 21
  IF ( T%R%KIND         .NE. T%K1         ) STOP 22
  IF ( ANY ( T%R        .NE. T%K1       ) ) STOP 23
  IF ( SIZE( T%I )      .NE. T%L2         ) STOP 24
  IF ( T%I%KIND         .NE. T%K2         ) STOP 25
  IF ( ANY ( T%I        .NE. T%K2       ) ) STOP 26
  IF ( SIZE( T%C )      .NE. T%L2         ) STOP 27
  IF ( T%C%LEN          .NE. T%L2         ) STOP 28
  IF ( ANY (T%C         .NE. CHAR(T%K2) ) ) STOP 29


  S =  DT1(1,3,4,5)([1,2,3,4,5]) 
  T =  DT2(1,3,4,5,8,7)(                  &
           DT1=DT1(1,3,4,5)([1,2,3,4,5]), &
             I=[1,2,3,4,5,6,7],           &
             C=CHAR([1,2,3,4,5,6,7]),     &
            Ptr=NULL() )                  



  IF ( ANY ( S%R        .NE. [1,2,3,4,5]           ) ) STOP 42

  IF ( ANY ( T%R        .NE. [1,2,3,4,5]           ) ) STOP 44
  IF ( ANY ( T%I        .NE. [1,2,3,4,5,6,7]       ) ) STOP 45
  IF ( ANY ( T%C        .NE. CHAR([1,2,3,4,5,6,7]) ) ) STOP 46

  END SUBROUTINE

  END

