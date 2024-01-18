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
!*  -- A variable specified by implicit typing rules
!*
!*   (ice-336523/336537)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  CONTAINS

  SUBROUTINE ModSub(L)
  IMPLICIT TYPE(DT(K=4,K1=1,K2=2,K4=4,K8=8,L=9))(D)
  INTEGER :: L

  TYPE :: DT(K,K1,K2,K4,K8,L)
    INTEGER,     LEN :: L=0
    INTEGER,    KIND :: K = 4
    INTEGER(8), KIND :: K1 = 1
    INTEGER(4), KIND :: K2 = 2
    INTEGER(2), KIND :: K4 = 4
    INTEGER(1), KIND :: K8 = 8
    CHARACTER(LEN=L) :: C
  END TYPE

  TYPE(DT(K=4, L=DTT%L)) :: T1(DTT%L)
  TYPE(DT(K =DTT1%K,  &
          K1=DTT1%K1, &
          K2=DTT1%K2, &
          K4=DTT1%K4, &
          K8=DTT1%K8, &
          L =DTT%L+DTT1%L)) :: T2(DTT1%L)


  IF (  T1%K        .NE.  4  )            ERROR STOP 20
  IF (  T1%K1       .NE. 1  )            ERROR STOP 21
  IF (  T1%K2       .NE. 2  )            ERROR STOP 22
  IF (  T1%K4       .NE. 4  )            ERROR STOP 23
  IF (  T1%K8       .NE. 8  )            ERROR STOP 24
  IF (  LEN( T1%C)  .NE.  9  )            ERROR STOP 25
  IF (  SIZE(T1)    .NE.  9  )            ERROR STOP 26


  IF (  T2%K        .NE.  4  )            ERROR STOP 30
  IF (  T2%K1       .NE.  1  )            ERROR STOP 31
  IF (  T2%K2       .NE.  2  )            ERROR STOP 32
  IF (  T2%K4       .NE.  4  )            ERROR STOP 33
  IF (  T2%K8       .NE.  8  )            ERROR STOP 34
  IF (  LEN( T2%C)  .NE.  18  )            ERROR STOP 35
  IF (  SIZE(T2)    .NE.  9 )            ERROR STOP 36


  END SUBROUTINE

  END MODULE


  PROGRAM dtParamTypeDecC501_14
  USE M

  CALL ModSub( 1 )

  END

