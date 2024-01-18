!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParamTypeDecC501_12
!*
!*  DATE                       : May. 03, 2007
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
!*  --  on subscript, section subscript, substring starting point, and
!*      substring ending point
!*
!*   (340466)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE :: DT0(K1,K2,K4,K8,L0)
    INTEGER,     LEN :: L0=0
    INTEGER(8), KIND :: K1 = 1
    INTEGER(4), KIND :: K2 = 2
    INTEGER(2), KIND :: K4 = 4
    INTEGER(1), KIND :: K8 = 8
    CHARACTER(LEN=L0):: C
  END TYPE

  INTEGER, PARAMETER  :: T0(8)=[1,2,3,4,5,6,7,8]
  TYPE(DT0) :: T1(100, 100)
  CHARACTER(100) :: C
  INTEGER :: I=1
  CONTAINS

  PURE FUNCTION ModFun(Arg)
  INTEGER :: ModFun
  INTEGER, INTENT(IN) :: Arg
    ModFun = Arg
  END FUNCTION

  END MODULE

  MODULE M1
  USE M
  TYPE :: DT(K,L)
    INTEGER, KIND :: K=4
    INTEGER, LEN  :: L=1
    TYPE(DT0(L0=T0(L)+T0(K)))             :: X1(T0(L):T0(K))
    TYPE(DT0(L0=SIZE(T1(:T0(L),:T0(K))))) :: X2(SIZE(T1(:T0(L),:T0(K))))
    TYPE(DT0(L0=LEN(C(T0(L):T0(K)))))     :: X3(LEN(C(T0(L):T0(K))))
  END TYPE

  CONTAINS

  SUBROUTINE ModSub(L)
  INTEGER :: L
  TYPE(DT(K=4, L=L)) :: T

  IF ( T%K        .NE.   4  )  STOP 11
  IF ( T%L        .NE.   2  )  STOP 12

  IF (  T%X1%K1       .NE.  1  )            STOP 11
  IF (  T%X1%K2       .NE.  2  )            STOP 12
  IF (  T%X1%K4       .NE.  4  )            STOP 13
  IF (  T%X1%K8       .NE.  8  )            STOP 14
  IF (  LEN( T%X1%C)  .NE.  T0(L)+T0(K))    STOP 15
  IF (  SIZE(T%X1%C)  .NE.  T0(K)-T0(L)+1)  STOP 16

  IF (  T%X2%K1       .NE.  1  )            STOP 21
  IF (  T%X2%K2       .NE.  2  )            STOP 22
  IF (  T%X2%K4       .NE.  4  )            STOP 23
  IF (  T%X2%K8       .NE.  8  )            STOP 24
  IF (  LEN( T%X2%C)  .NE.  SIZE(T1(:T0(L),:T0(K)))  )  STOP 25
  IF (  SIZE(T%X2%C)  .NE.  SIZE(T1(:T0(L),:T0(K)))  )  STOP 26

  IF (  T%X3%K1       .NE.  1  )            STOP 31
  IF (  T%X3%K2       .NE.  2  )            STOP 32
  IF (  T%X3%K4       .NE.  4  )            STOP 33
  IF (  T%X3%K8       .NE.  8  )            STOP 34
  IF (  LEN( T%X3%C)  .NE.  LEN(C(T0(L):T0(K)))  )  STOP 35
  IF (  SIZE(T%X3%C)  .NE.  LEN(C(T0(L):T0(K)))  )  STOP 36


  END SUBROUTINE

  END MODULE


  PROGRAM dtParamTypeDecC501_12
  USE M1

  CALL ModSub( 2 )

  END

