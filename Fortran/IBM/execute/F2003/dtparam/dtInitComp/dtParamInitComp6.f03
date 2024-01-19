!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : May. 29, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : DERIVED TYPE PARAMETERS
!*
!*  SECONDARY FUNCTIONS TESTED : Default initialization for component
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
!*  ZERO sized array
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM dtParamInitComp6

  TYPE :: DT(K, L)
    INTEGER(1), KIND :: K=4
    INTEGER(1), LEN  :: L=1
    SEQUENCE
    INTEGER(K)   :: I=K
    REAL(K)      :: R=K
    COMPLEX(K)   :: Z=(K,-K)
    CHARACTER(L) :: C="!!!!"
    LOGICAL(K)   :: LL=.TRUE._1
  END TYPE

  TYPE(DT)  :: T(3:4, 1:0)
  TYPE(DT(L=1,K=8))  :: T1(3:4, 1:0)


  IF ( T%K         .NE. 4 )                ERROR STOP 11
  IF ( T%L         .NE. 1 )                ERROR STOP 12

  IF ( KIND(T%I)   .NE. 4 )                ERROR STOP 13
  IF ( SIZE(T%I)   .NE. 0 )                ERROR STOP 14

  IF ( KIND(T%R)   .NE. 4 )                ERROR STOP 15
  IF ( SIZE(T%R)   .NE. 0 )                ERROR STOP 16

  IF ( KIND(T%Z)   .NE. 4 )                ERROR STOP 17
  IF ( SIZE(T%Z)   .NE. 0 )                ERROR STOP 18

  IF ( LEN(T%C)    .NE. 1 )                ERROR STOP 21
  IF ( SIZE(T%C)   .NE. 0 )                ERROR STOP 22

  IF ( KIND(T%LL)  .NE. 4 )                ERROR STOP 23
  IF ( SIZE(T%LL)  .NE. 0 )                ERROR STOP 24


  IF ( T1%K        .NE. 8 )                ERROR STOP 31
  IF ( T1%L        .NE. 1 )                ERROR STOP 32

  IF ( KIND(T1%I)  .NE. 8 )                ERROR STOP 33
  IF ( SIZE(T1%I)  .NE. 0 )                ERROR STOP 34

  IF ( KIND(T1%R)  .NE. 8 )                ERROR STOP 35
  IF ( SIZE(T1%R)  .NE. 0 )                ERROR STOP 36

  IF ( KIND(T1%Z)  .NE. 8 )                ERROR STOP 37
  IF ( SIZE(T1%Z)  .NE. 0 )                ERROR STOP 38

  IF ( LEN(T1%C)   .NE. 1 )                ERROR STOP 41
  IF ( SIZE(T1%C)  .NE. 0 )                ERROR STOP 42

  IF ( KIND(T1%LL) .NE. 8 )                ERROR STOP 43
  IF ( SIZE(T1%LL) .NE. 0 )                ERROR STOP 44


  END

