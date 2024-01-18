!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : May. 04, 2007
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
!*  -- Finalization
!*
!*   ()
!  NOTE: JX: the finalization is supposed to happen; but it's down to the lowest
!  priority for compiler to implement it. Ignore it for now (2008-06-11).
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE :: DT(K,K1,K2,K4,K8,L)
    INTEGER,     LEN :: L=0
    INTEGER,    KIND :: K = 4
    INTEGER(8), KIND :: K1 = 1
    INTEGER(4), KIND :: K2 = 2
    INTEGER(2), KIND :: K4 = 4
    INTEGER(1), KIND :: K8 = 8
    CHARACTER(LEN=L) :: C
    CONTAINS
    FINAL :: Final1, Final2
  END TYPE

  LOGICAL :: LFinal1=.true.
  LOGICAL :: LFinal2=.true.

  CONTAINS

  SUBROUTINE Final1(Arg)
  TYPE(DT(L=*)) :: Arg
    PRINT *, "Finalization1 processed!"
    LFinal1 = .true.
  END SUBROUTINE

  SUBROUTINE Final2(Arg)
  TYPE(DT(K=2,L=*)) :: Arg(10)
    PRINT *, "Finalization10 processed!"
    LFinal2 = .true.
  END SUBROUTINE

  SUBROUTINE ModSub(L)
  INTEGER :: L
  TYPE(DT(K=4, L=SIZE([DT(L=L)("?")]))) :: T1
  TYPE(DT(K=4, L=SIZE([(DT(K=2)(''),i=1,10)]))) :: T2

  IF ( .NOT. LFinal1   )  STOP 11
  IF ( .NOT. LFinal2   )  STOP 12

  IF (  T1%K        .NE.  4  )            STOP 20
  IF (  T1%K1       .NE.  1  )            STOP 21
  IF (  T1%K2       .NE.  2  )            STOP 22
  IF (  T1%K4       .NE.  4  )            STOP 23
  IF (  T1%K8       .NE.  8  )            STOP 24
  IF (  LEN( T1%C)  .NE.  1  )            STOP 25


  IF (  T2%K        .NE.  4  )            STOP 30
  IF (  T2%K1       .NE.  1  )            STOP 31
  IF (  T2%K2       .NE.  2  )            STOP 32
  IF (  T2%K4       .NE.  4  )            STOP 33
  IF (  T2%K8       .NE.  8  )            STOP 34
  IF (  LEN( T2%C)  .NE.  10 )            STOP 35


  END SUBROUTINE

  END MODULE


  PROGRAM dtParamTypeDecC501_13
  USE M

  CALL ModSub( 1 )

  END

