! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2011-08-25
!*  ORIGIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   : Copy-in/out for assumed shape ptrays
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                :
!*
!*    - Actual argument is of derived type
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012
MODULE Mod
      IMPLICIT NONE
      INTEGER, PARAMETER :: N=100, N2=2

      TYPE :: Base
         INTEGER :: id = -1
         CHARACTER(N) :: name = "XLFtest"
         INTEGER :: arr(N) = -99
      END TYPE

      CONTAINS

      SUBROUTINE Sub1(arg1,arg2)
        INTEGER, CONTIGUOUS, INTENT(IN) :: arg1(:), arg2(:)
        INTEGER :: obj(size(arg1)), I

        IF ( .NOT. IS_CONTIGUOUS(arg1) ) ERROR STOP 20
        IF ( .NOT. IS_CONTIGUOUS(arg2) ) ERROR STOP 21
        IF ( .NOT.  IS_CONTIGUOUS(obj) ) ERROR STOP 22

        DO I = 1, size(arg1)
          IF ( arg1(I)    /=      7*(I-1)+1 ) ERROR STOP 23
          IF ( arg2(I)    /=  -99+(7*(I-1)) ) ERROR STOP 24
          obj(I) = arg1(I) + arg2(size(arg1)-I+1)
        ENDDO
        IF ( any(obj /= 0) ) ERROR STOP 25
      END SUBROUTINE Sub1

      FUNCTION FUN(arg1,arg2) RESULT(res)
        INTEGER, CONTIGUOUS, INTENT(INOUT) :: arg1(:), arg2(:)
        INTEGER :: res(size(arg1)), I

        IF ( .NOT. IS_CONTIGUOUS(arg1) ) ERROR STOP 30
        IF ( .NOT. IS_CONTIGUOUS(arg2) ) ERROR STOP 31
        IF ( .NOT.  IS_CONTIGUOUS(res) ) ERROR STOP 32

        DO I = 1, size(arg1)
          IF ( arg1(I)    /=      25*(I-1)+1 ) ERROR STOP 33
          IF ( arg2(I)    /=  -99+(25*(I-1)) ) ERROR STOP 34
          res(I) = arg1(I) - arg2(I)
        ENDDO
        IF ( any(res /= 100) ) ERROR STOP 35
        CALL SubSub(arg1(::2))
      END FUNCTION FUN

      SUBROUTINE SubSub(Arg)
        INTEGER :: Arg(2)

        IF ( .NOT. IS_CONTIGUOUS(Arg) ) ERROR STOP 40
        IF ( ANY(Arg   .NE.   [1,51]) ) ERROR STOP 41
        IF ( Arg(1)    .NE.         1 ) ERROR STOP 42
        IF ( Arg(2)    .NE.        51 ) ERROR STOP 43
      END SUBROUTINE SubSub

      INTEGER FUNCTION Foo(I)
         INTEGER :: I
         foo = I -100
      END FUNCTION

END MODULE Mod
PROGRAM CopyInOut4
      USE Mod
      IMPLICIT NONE
      INTEGER         :: I
      TYPE(Base), TARGET  :: b1(N)
      CLASS(*), POINTER    :: upoly(:)
      CLASS(Base), POINTER :: b_ptr(:)

      IF ( any(b1%id      /=        -1) ) ERROR STOP 100
      IF ( any(b1%name    /= 'XLFtest') ) ERROR STOP 101
      DO I = 1, N
           IF ( any(b1%arr(I) /=  -99 ) ) ERROR STOP 102
      ENDDO

      DO I = 1, N
         b1(I)%id   = I
         IF ( I == 1 ) THEN
               b1(I)%name = "A"
         ELSE
               b1(I)%name = "A"//b1(I-1)%name
         ENDIF
         b1(I)%arr = foo(I)
      ENDDO

      b_ptr => b1(1:N:7)
      CALL Sub1(b_ptr%id,b_ptr%arr(1))

      upoly => b_ptr
      SELECT TYPE ( upoly )
        TYPE IS (Base)
           CALL Sub1(upoly%id,b_ptr%arr(1))

        CLASS DEFAULT
          STOP 200
      END SELECT

      b_ptr => b1(1:N:25)
      IF ( any(fun(b_ptr%id,b_ptr%arr(1)) /= 100) ) ERROR STOP 103

      upoly => b_ptr
      SELECT TYPE ( upoly )
        TYPE IS (Base)
           IF ( any(fun(upoly%id,upoly%arr(1)) /= 100) ) ERROR STOP 104

        CLASS DEFAULT
          STOP 201
      END SELECT

END PROGRAM CopyInOut4
