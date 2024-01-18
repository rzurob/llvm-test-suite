!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrSubscriptOrder.f
!*
!*  DATE                       : Feb. 20, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 289075
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  SubscriptOrder
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM dataPtrSubscriptOrder
  !IBM* SUBSCRIPTORDER(Tar2(2,1), Tar1(1))
  IMPLICIT NONE

  REAL,  TARGET  :: Tar2(10, 100)
  REAL,  TARGET  :: Tar1(1000)
  REAL,  POINTER :: Ptr(:, :)
  INTEGER    :: I, J, K, N1, N2

  !N1 = 100; N2 = 10; K = 0
  N2 = 100; N1 = 10; K = 0

  Tar2 = RESHAPE((/((i*J,i=1,N2), j=1, N1)/), (/N2, N1/))
  Tar1 = (/(i,i=1,N1*N2)/)

  DO I =1, N2     ! The shape of Tar2 now is (N2, N1)
  DO J =I, N1

    Ptr(I:, J:) => Tar2
    IF ( ANY( MAXLOC(Ptr)  .NE. (/N2, N1/)) )  STOP 11
    IF ( ANY( MINLOC(Ptr)  .NE. (/1, 1/)) )    STOP 12
    CALL Check2()

    Ptr(I:J, I:J) => Tar1
    IF ( ANY( MAXLOC(Ptr)  .NE. (/J-I+1, J-I+1/)) )  STOP 21
    IF ( ANY( MINLOC(Ptr)  .NE. (/1, 1/)) )          STOP 22
    CALL Check1()

  END DO
  END DO

  CONTAINS

  SUBROUTINE Check1()
    IF (SIZE(Ptr)  .NE. (J-I+1)*(J-I+1))            STOP 30
    IF (.NOT. ASSOCIATED(Ptr))                      STOP 31
    IF (ANY( LBOUND(Ptr) .NE. (/I,  I/)))           STOP 32
    IF (ANY( UBOUND(Ptr) .NE. (/J,  J/)))           STOP 33
  END SUBROUTINE

  SUBROUTINE Check2()
    IF (SIZE(Ptr)  .NE. N2*N1 )                    STOP 40
    IF (.NOT. ASSOCIATED(Ptr, Tar2))               STOP 41
    IF (ANY( LBOUND(Ptr) .NE. (/I, J /)))          STOP 42
    IF (ANY( UBOUND(Ptr) .NE. (/I+N2-1, J+N1-1/))) STOP 43
  END SUBROUTINE

  END



