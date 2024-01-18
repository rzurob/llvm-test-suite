!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : May. 07, 2007
!*
!*  PRIMARY FUNCTIONS TESTED   : DERIVED TYPE PARAMETERS
!*
!*  SECONDARY FUNCTIONS TESTED : Data Object Declaration
!*
!*  REFERENCE                  : Feature Number 289057
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  C501 (R501) In a declaration-type-spec, every type-param-value that is
!*  not a colon or an asterisk shall be a specification-expr
!*
!*  -- Inquiry on entities defined previously
!*
!*   (340498)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  CONTAINS

  SUBROUTINE ModSub(L)
  INTEGER :: L

  TYPE :: DT(K,K1,K2,K4,K8,L)
    INTEGER,     LEN :: L=0
    INTEGER,    KIND :: K = 4
    INTEGER(8), KIND :: K1 = 1
    INTEGER(4), KIND :: K2 = 2
    INTEGER(2), KIND :: K4 = 4
    INTEGER(1), KIND :: K8 = 8
    CHARACTER(LEN=L) :: C(L)
  END TYPE

  TYPE(DT(K=4,K1=8,K2=4,K4=2,K8=1, L=L)) :: T(1:L)
  TYPE(DT(K=T%K, K1=T%K1, K2=T%K2, K4=T%K4, K8=T%K8,  L=T%L))  :: T1(1:UBOUND(T,1))
  TYPE(DT(K=T%K, K1=T1%K1,K2=T1%K2,K4=T1%K4,K8=T1%K8, L=T1%L)) :: T2(1:UBOUND(T1,1)), T3(UBOUND(T2,1))


  IF (  T1%K          .NE.  4  )            STOP 20
  IF (  T1%K1         .NE.  8  )            STOP 21
  IF (  T1%K2         .NE.  4  )            STOP 22
  IF (  T1%K4         .NE.  2  )            STOP 23
  IF (  T1%K8         .NE.  1  )            STOP 24;print*, LEN( T1(1)%C)
  IF (  T1%L          .NE.  L  )            STOP 25
  IF (  LEN( T1(1)%C) .NE.  L  )            STOP 26
  IF (  SIZE(T1(2)%C) .NE.  L  )            STOP 27
  IF (  SIZE(T1)      .NE.  L  )            STOP 28


  IF (  T3%K           .NE.  4  )            STOP 30
  IF (  T3%K1          .NE.  8  )            STOP 31
  IF (  T3%K2          .NE.  4  )            STOP 32
  IF (  T3%K4          .NE.  2  )            STOP 33
  IF (  T3%K8          .NE.  1  )            STOP 34
  IF (  T3%L           .NE.  L  )            STOP 25
  IF (  LEN( T3(1)%C)  .NE.  L  )            STOP 36
  IF (  SIZE(T3(2)%C)  .NE.  L  )            STOP 37
  IF (  SIZE(T3)       .NE.  L  )            STOP 38


  END SUBROUTINE

  END MODULE


  PROGRAM dtParamTypeDecC501_15
  USE M

  CALL ModSub( 6 )

  END

