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
!*  BLOCK DATA
!*
!*  (340424)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  BLOCK DATA

! C589 (R558) If a common-block-object is of a derived type, it shall be a
! sequence type (4.5.1) or a  type with the BIND attribute and it shall
! have no default initialization.
    TYPE :: DT(K, L)
      INTEGER(1), KIND :: K=0
      INTEGER(1), LEN  :: L=0
      SEQUENCE
      INTEGER(K)   :: I(L) !=(/(K, I=1,K)/) !Violation of C589
      REAL(K)      :: R(L) !=(/(K, I=1,K)/)
      COMPLEX(K)   :: Z(L) !=(/((K,-K), I=1,K)/)
      CHARACTER(L) :: C(L) !="!!!!"
      LOGICAL(K)   :: LL(L) !=.TRUE._1
    END TYPE

    TYPE(DT(L=4,K=4))  :: T(3)

    TYPE(DT(K=4,L=4))                :: TT(3) =   &
              DT(4,4) (                           &
              I=(/(-T%K, I=1,T%K)/),              &
              R=(/(-T%K, I=1,T%K)/),              &
              Z=(/((-T%K,T%K), I=1,T%K)/),        &
              C="??????????",                     &
              LL=(/(.FALSE._4, I=1,T%L)/)         )


    COMMON /CB/ TT

  END BLOCK DATA


  PROGRAM dtParamInitComp5

  TYPE :: DT(K, L)
    INTEGER(1), KIND :: K=0
    INTEGER(1), LEN  :: L=0
    SEQUENCE
    INTEGER(K)   :: I(L)
    REAL(K)      :: R(L)
    COMPLEX(K)   :: Z(L)
    CHARACTER(L) :: C(L)
    LOGICAL(K)   :: LL(L)
  END TYPE

  TYPE(DT(L=4,K=4))  :: T(3)

  COMMON /CB/ T

  IF ( T%K         .NE. 4 )                  STOP 43
  IF ( T%L         .NE. 4 )                  STOP 44

  DO I =1, 3

    IF ( T(I)%K         .NE. 4 )                  STOP 13
    IF ( T(I)%L         .NE. 4 )                  STOP 14

    IF ( KIND(T(I)%I)   .NE. 4 )                  STOP 15
    IF ( SIZE(T(I)%I)   .NE. 4 )                  STOP 16
    IF ( ANY(T(I)%I     .NE. (/(-T%K, I=1,4)/)))  STOP 17

    IF ( KIND(T(I)%R)   .NE. 4 )                  STOP 18
    IF ( SIZE(T(I)%R)   .NE. 4 )                  STOP 19
    IF ( ANY(T(I)%R     .NE. (/(-T%K, I=1,4)/)))  STOP 20

    IF ( KIND(T(I)%Z)   .NE. 4 )                      STOP 21
    IF ( SIZE(T(I)%Z)   .NE. 4 )                      STOP 22
    IF ( ANY(T(I)%Z     .NE. (/((-T%K,T%K),I=1,4)/))) STOP 23

    IF ( LEN(T(I)%C)    .NE. 4 )                STOP 24
    IF ( SIZE(T(I)%C)   .NE. 4 )                STOP 25
    IF ( ANY(T(I)%C     .NE. "????" ))          STOP 26

    IF ( KIND(T(I)%LL)  .NE. 4 )                STOP 28
    IF ( SIZE(T(I)%LL)  .NE. 4 )                STOP 29
    IF ( ANY(T(I)%LL    .NEQV. .FALSE._4))      STOP 30

  END DO

  END

