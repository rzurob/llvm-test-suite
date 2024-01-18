!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : May. 17, 2007
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
!*  Type Specifier used in function
!*
!*  Host association
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

  TYPE :: DT(K,L)
    INTEGER, KIND :: K=4
    INTEGER, LEN  :: L=1
    INTEGER(K)    :: I=K
    CHARACTER(L)  :: C(L)
  END TYPE

  TYPE, EXTENDS(DT) :: DT1(K1,L1)
    INTEGER, KIND :: K1=K
    INTEGER, LEN  :: L1=K
  END TYPE

  CONTAINS

  FUNCTION IntFun1(I,C)
    CHARACTER(*) C
    INTEGER  I
    TYPE(DT(2,LEN(C))) intFun1
    IntFun1 = DT(2,LEN(C))(C=C, I=I)
  END FUNCTION

  FUNCTION IntFun2(D)
    TYPE(DT1(4,*, K1=2, L1=*)), TARGET :: D
    POINTER IntFun2

    CLASS(DT(4,LEN(D%C))) IntFun2
    IntFun2 => D
  END FUNCTION

  CLASS(*) FUNCTION IntFun3(I,C)
  CHARACTER(*) C
  INTEGER      I
  ALLOCATABLE  IntFun3
    ALLOCATE(IntFun3, SOURCE=DT1(I%KIND,LEN(C), K1=2, L1=LEN(C))(I,C))
  END FUNCTION

  END MODULE

  PROGRAM dtpDecTypeSpecifier4Func
  USE M

  TYPE(DT(2,1)) :: B = DT(2,1)(C="X", I=-1)
  TYPE(DT1(4,2, K1=2, L1=2)), TARGET :: D = DT1(4,2, K1=2, L1=2)(-1, "XY")
  CLASS(DT(4,2)), POINTER     :: C

  B = IntFun1(C="X", I=-1)

  IF ( B%K               .NE.   2          ) ERROR STOP 11
  IF ( B%L               .NE.   1          ) ERROR STOP 12
  IF ( B%I%KIND          .NE.   2          ) ERROR STOP 13
  IF ( B%I               .NE.  -1          ) ERROR STOP 14
  IF ( B%C%LEN           .NE.   1          ) ERROR STOP 15
  IF ( SIZE(B%C)         .NE.   1          ) ERROR STOP 16
  IF ( ANY(B%C           .NE.   "X"      ) ) ERROR STOP 17

  C => IntFun2(D)

  SELECT TYPE (C)
  TYPE IS (DT1(4,*, K1=2, L1=*))

    IF ( C%K             .NE.   4          ) ERROR STOP 21
    IF ( C%L             .NE.   2          ) ERROR STOP 22
    IF ( C%K1            .NE.   2          ) ERROR STOP 23
    IF ( C%L1            .NE.   2          ) ERROR STOP 24
    IF ( C%I%KIND        .NE.   4          ) ERROR STOP 25
    IF ( C%I             .NE.  -1          ) ERROR STOP 26
    IF ( C%C%LEN         .NE.   2          ) ERROR STOP 27
    IF ( SIZE(C%C)       .NE.   2          ) ERROR STOP 28
    IF ( ANY(C%C         .NE.   "XY"     ) ) ERROR STOP 29

  CLASS DEFAULT
    STOP 20
  END SELECT

  SELECT TYPE (E => IntFun3(-1, "XY"))
  TYPE IS (DT1(4,*, 2, *))

    IF ( E%K             .NE.   4          ) ERROR STOP 31
    IF ( E%L             .NE.   2          ) ERROR STOP 32
    IF ( E%K1            .NE.   2          ) ERROR STOP 33
    IF ( E%L1            .NE.   2          ) ERROR STOP 34
    IF ( E%I%KIND        .NE.   4          ) ERROR STOP 35
    IF ( E%I             .NE.  -1          ) ERROR STOP 36
    IF ( E%C%LEN         .NE.   2          ) ERROR STOP 37
    IF ( SIZE(E%C)       .NE.   2          ) ERROR STOP 38
    IF ( ANY(E%C         .NE.   "XY"     ) ) ERROR STOP 39

  CLASS DEFAULT
    STOP 30
  END SELECT

  END

