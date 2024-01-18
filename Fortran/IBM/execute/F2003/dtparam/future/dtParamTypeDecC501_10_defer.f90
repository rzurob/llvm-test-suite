!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParamTypeDecC501_10
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
!*  -- An ac-do-variable within an array constructor where each scalar-int-expr
!*     of the corresponding ac-implied-do-control is a restricted expression
!*
!*   (340460)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE :: DT0(L0)
    INTEGER, LEN :: L0=0
  END TYPE

  TYPE :: DT(K,L)
    INTEGER, KIND :: K=4
    INTEGER, LEN  :: L=1
    CHARACTER(LEN=L)  :: C
    TYPE(DT(K, L=SIZE([(I,I=1,L)]))),                            POINTER :: Ptr1
    TYPE(DT(K, L=SIZE([(DT0(L0=I)(),      I=1, L+1)]))),         POINTER :: Ptr2    => NULL()
    TYPE(DT(K, L=SIZE([(SUM([I]),         I=1, L+2)]))),         POINTER :: Ptr3(:) => NULL()
  END TYPE

  CONTAINS

  PURE FUNCTION ModFun(Arg)
  INTEGER :: ModFun
  INTEGER, INTENT(IN) :: Arg
    ModFun = Arg
  END FUNCTION

  SUBROUTINE ModSub(L)
  INTEGER :: L
  TYPE(DT(K=4, L=L)) :: T
  TYPE(DT(K=4, L=SIZE([(DT0(L0=ModFun(I))(), I=1, ModFun(L+3))]))), POINTER :: Ptr4(:)

  nullify (t%ptr1)

  IF ( T%K        .NE.   4  )  STOP 11
  IF ( T%L        .NE.   2  )  STOP 12
  IF ( T%C%LEN    .NE.   2  )  STOP 13

  IF (  T%Ptr1%K   .NE.  4  )  STOP 21
  IF (  T%Ptr1%L   .NE.  2  )  STOP 22

  IF (  T%Ptr2%K   .NE.  4  )  STOP 31
  IF (  T%Ptr2%L   .NE.  3  )  STOP 32

  IF (  T%Ptr3%K   .NE.  4  )  STOP 41
  IF (  T%Ptr3%L   .NE.  4  )  STOP 42

  IF (    Ptr4%K   .NE.  4  )  STOP 51
  IF (    Ptr4%L   .NE.  5  )  STOP 52


  END SUBROUTINE

  END MODULE


  PROGRAM dtParamTypeDecC501_10
  USE M

  CALL ModSub( 2 )

  END

