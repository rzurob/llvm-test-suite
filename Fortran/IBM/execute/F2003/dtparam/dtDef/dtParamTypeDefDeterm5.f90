!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParamTypeDefDeterm5   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Dec. 15, 2005
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : DERIVED TYPE PARAMETERS
!*
!*  SECONDARY FUNCTIONS TESTED : Determination of Types 
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
!*  Determination of derived types - sequence types 
!*
!*  (Syntax err&ice/340207)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE :: DT(K, L)
    INTEGER, KIND :: K
    INTEGER, LEN  :: L
    SEQUENCE      
    INTEGER(K)  :: I
    REAL(K)     :: R
    COMPLEX(K)  :: Cplx
    LOGICAL(K)  :: LL
    CHARACTER(L):: C
  END TYPE

  TYPE(DT(8, 4)), TARGET      :: T0, TT0
  TYPE(DT(8, :)), ALLOCATABLE :: T1, TT1
  TYPE(DT(8, :)), POINTER     :: T2, TT2

  END MODULE

  PROGRAM dtParamTypeDefDeterm5 
  USE M, ONLY : T0, T1, T2
  IMPLICIT TYPE(DT(8, 4))(T)

  TYPE :: DT(K, L)
    INTEGER, KIND :: K
    INTEGER, LEN  :: L
    SEQUENCE
    INTEGER(K)  :: I
    REAL(K)     :: R
    COMPLEX(K)  :: Cplx
    LOGICAL(K)  :: LL
    CHARACTER(L):: C
  END TYPE

  TARGET      :: TT0
  ALLOCATABLE :: TT1
  POINTER     :: TT2

  TT0 = DT(8,4)(-1_8, -1.0_8, (1._8, -1._8), .FALSE._8, "B")

  T0 = TT0
  IF ( T0%I      .NE.   -1_8 )           STOP 23
  IF ( T0%R      .NE.   -1.0_8 )         STOP 24
  IF ( T0%Cplx   .NE.   (1._8, -1._8) )  STOP 25
  IF ( T0%LL     .NEQV. .FALSE._8 )      STOP 26
  IF ( TRIM(T0%C).NE.   TRIM("B"))       STOP 27

  ALLOCATE(DT(8,4) :: T1)
  T1 = TT0

  IF ( T1%I      .NE.   -1_8 )           STOP 33
  IF ( T1%R      .NE.   -1.0_8 )         STOP 34
  IF ( T1%Cplx   .NE.   (1._8, -1._8) )  STOP 35
  IF ( T1%LL     .NEQV. .FALSE._8 )      STOP 36
  IF ( TRIM(T1%C).NE.   TRIM("B"))       STOP 37

  IF ( T1%I      .NE.   -1_8 )           STOP 33
  IF ( T1%R      .NE.   -1.0_8 )         STOP 34
  IF ( T1%Cplx   .NE.   (1._8, -1._8) )  STOP 35
  IF ( T1%LL     .NEQV. .FALSE._8 )      STOP 36
  IF ( TRIM(T1%C).NE.   TRIM("B"))       STOP 37

  T2 => TT0

  IF ( T2%I      .NE.   -1_8 )           STOP 43
  IF ( T2%R      .NE.   -1.0_8 )         STOP 44
  IF ( T2%Cplx   .NE.   (1._8, -1._8) )  STOP 45
  IF ( T2%LL     .NEQV. .FALSE._8 )      STOP 46
  IF ( TRIM(T0%C).NE.   TRIM("B"))       STOP 47

  TT2 => T2

  IF ( TT2%I      .NE.   -1_8 )           STOP 53
  IF ( TT2%R      .NE.   -1.0_8 )         STOP 54
  IF ( TT2%Cplx   .NE.   (1._8, -1._8) )  STOP 55
  IF ( TT2%LL     .NEQV. .FALSE._8 )      STOP 56
  IF ( TRIM(TT2%C).NE.   TRIM("B"))       STOP 57

  END

