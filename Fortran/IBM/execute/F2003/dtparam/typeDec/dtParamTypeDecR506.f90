!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParamTypeDecR506
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : May. 09, 2007
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
!*  R506 initialization is = initialization-expr
!*                         or => null-init 
!*
!*  (336635/340788/345390) 
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dtParamTypeDecR506

  TYPE :: DT0(K,L)
    INTEGER, KIND :: K=4
    INTEGER, LEN  :: L=1
    INTEGER       :: I=K
    CHARACTER(L)  :: C0=CHAR(K)
  END TYPE

  TYPE, EXTENDS(DT0) :: DT(KIND, LEN)
    INTEGER(1), KIND     :: KIND=K
    INTEGER(2), LEN      :: LEN=K-3
    REAL(KIND)           :: R=KIND
    CHARACTER(LEN)       :: C=CHAR(KIND)
    TYPE(DT0(KIND, LEN)) :: T !=DT0(KIND, LEN)()
  END TYPE


  TYPE(DT(KIND=4, LEN=1)), PARAMETER :: T1(10) = DT(I=-4, C0="A", R=-4, C="B", T=DT0()) 

  TYPE(DT(4,     LEN=T1%LEN))        :: T2(SIZE(T1))  =  [[[T1]]] 
  TYPE(DT(4,       L=T1%L)), TARGET  :: T3(SIZE(T1))  =  [(T1(i), I=1, SIZE(T1))] 
  TYPE(DT(4,       L=T1%L)), POINTER :: T4 => NULL() 
  TYPE(DT(4,       L=T1%L)), POINTER :: T5(:) => NULL() 


  IF ( T1%K               .NE.   4          ) STOP 11
  IF ( T1%L               .NE.   1          ) STOP 12
  IF ( T1%KIND            .NE.   4          ) STOP 13
  IF ( T1%LEN             .NE.   1          ) STOP 14
  IF ( LEN( T1%C0 )       .NE.   1          ) STOP 15
  IF ( ANY( T1%C0         .NE.   "A"      ) ) STOP 16
  IF ( ANY( T1%I          .NE.   -4       ) ) STOP 17
  IF ( ANY( T1%R          .NE.   -4       ) ) STOP 18
  IF ( LEN( T1%C )        .NE.   1          ) STOP 19
  IF ( ANY( T1%C          .NE.   "B"      ) ) STOP 21
  IF ( T1%T%K             .NE.   4          ) STOP 22
  IF ( T1%T%L             .NE.   1          ) STOP 23
  IF ( ANY( T1%T%I        .NE.   4        ) ) STOP 24
  IF ( ANY( T1%T%C0       .NE.   CHAR(4)  ) ) STOP 25

  IF ( T2%K               .NE.   T1%K       ) STOP 31
  IF ( T2%L               .NE.   T1%L       ) STOP 32
  IF ( T2%KIND            .NE.   T1%KIND    ) STOP 33
  IF ( T2%LEN             .NE.   T1%LEN     ) STOP 34
  IF ( LEN( T2%C0 )       .NE.   LEN( T1%C0)) STOP 35
  IF ( ANY( T2%C0         .NE.   T1%C0    ) ) STOP 36
  IF ( ANY( T2%I          .NE.   T1%I     ) ) STOP 37
  IF ( ANY( T2%R          .NE.   T1%R     ) ) STOP 38
  IF ( LEN( T2%C )        .NE.   LEN(T1%C ) ) STOP 39
  IF ( ANY( T2%C          .NE.   T1%C     ) ) STOP 41
  IF ( T2%T%K             .NE.   T1%T%K     ) STOP 42
  IF ( T2%T%L             .NE.   T1%T%L     ) STOP 43
  IF ( ANY( T2%T%I        .NE.   T1%T%I   ) ) STOP 44
  IF ( ANY( T2%T%C0       .NE.   T1%T%C0  ) ) STOP 45

  IF ( T3%K               .NE.   T1%K       ) STOP 51
  IF ( T3%L               .NE.   T1%L       ) STOP 52
  IF ( T3%KIND            .NE.   T1%KIND    ) STOP 53
  IF ( T3%LEN             .NE.   T1%LEN     ) STOP 54
  IF ( LEN( T3%C0 )       .NE.   LEN( T1%C0)) STOP 55
  IF ( ANY( T3%C0         .NE.   T1%C0    ) ) STOP 56
  IF ( ANY( T3%I          .NE.   T1%I     ) ) STOP 57
  IF ( ANY( T3%R          .NE.   T1%R     ) ) STOP 58
  IF ( LEN( T3%C )        .NE.   LEN(T1%C ) ) STOP 59
  IF ( ANY( T3%C          .NE.   T1%C     ) ) STOP 61
  IF ( T3%T%K             .NE.   T1%T%K     ) STOP 62
  IF ( T3%T%L             .NE.   T1%T%L     ) STOP 63
  IF ( ANY( T3%T%I        .NE.   T1%T%I   ) ) STOP 64
  IF ( ANY( T3%T%C0       .NE.   T1%T%C0  ) ) STOP 65

  IF ( ASSOCIATED(T4) ) STOP 70
  IF ( ASSOCIATED(T5) ) STOP 71

  T5 => T3
  IF ( .NOT. ASSOCIATED(T5, T3) ) STOP 80
  T4 => T5(1)
  IF ( .NOT. ASSOCIATED(T4, T5(1)) ) STOP 81

  END

