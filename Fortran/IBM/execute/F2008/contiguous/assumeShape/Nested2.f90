! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-10-25
!*  ORIGIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   : Nested subroutine call
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                :
!*
!*    - Inner most subroutine has assumed shape array dummy argument
!*      with CONTIGUOUS attribute
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
PROGRAM Nested1
      IMPLICIT NONE

      INTEGER :: I, J, K
      INTEGER, TARGET  :: I3D(10,10,10)
      INTEGER, POINTER :: ptr(:,:,:)

      I3D = RESHAPE( SOURCE = [(I, I=1,1000)], SHAPE = [10,10,10] )
      ptr => I3D

      CALL Sub(ptr)

      DO K = 1, 10
         DO J = 1, 10
           DO I = 1, 10
               IF ( ptr(I, J, K) .NE. I+J+K ) ERROR STOP 10
           ENDDO
         ENDDO
       ENDDO

      CALL Sub()

      ptr => NULL()
      ALLOCATE( ptr(10,10,10), SOURCE=RESHAPE(SOURCE =[(I, I=1,1000)], SHAPE=[10,10,10]) )
      CALL Sub(ptr)

      DO K = 1, 10
         DO J = 1, 10
           DO I = 1, 10
               IF ( ptr(I, J, K) .NE. I+J+K ) ERROR STOP 12
           ENDDO
         ENDDO
       ENDDO

      CONTAINS

      SUBROUTINE Sub(Arg)
        INTEGER, TARGET, OPTIONAL, CONTIGUOUS :: Arg(:,:,:)
        INTEGER, POINTER :: Ptr(:,:,:)
        INTEGER :: test

        IF (PRESENT(Arg)) THEN
            IF ( .NOT. IS_CONTIGUOUS(Arg) ) ERROR STOP 20
            ptr => ARG
        ELSE
            ALLOCATE( ptr(10,10,10), SOURCE=RESHAPE(SOURCE=[(I, I=1,1000)], SHAPE=[10,10,10]))
        ENDIF

        IF ( .NOT. ASSOCIATED(ptr)    ) ERROR STOP 21
        IF ( .NOT. IS_CONTIGUOUS(ptr) ) ERROR STOP 22

        CALL SubSub(ptr, test)

        IF ( .NOT. ASSOCIATED(ptr)    ) ERROR STOP 23
        IF ( .NOT. IS_CONTIGUOUS(ptr) ) ERROR STOP 24
        IF (     test .NE. 16500      ) ERROR STOP 25

        IF (.NOT. PRESENT(Arg)) DEALLOCATE(ptr)
      END SUBROUTINE Sub

      SUBROUTINE SubSub(Arg, x)
        INTEGER :: Arg(10,10,10), x

        IF ( .NOT. IS_CONTIGUOUS(Arg) ) ERROR STOP 30
        IF ( ANY(Arg .NE. RESHAPE(SOURCE=[(I, I=1,1000)], SHAPE=[10,10,10])) ) ERROR STOP 31

        DO K = 1, 10
          DO J = 1, 10
            DO I = 1, 10
                Arg(I, J, K) = I+J+K
            ENDDO
          ENDDO
        ENDDO

       CALL inner(Arg, x)

      END SUBROUTINE SubSub

      SUBROUTINE inner(Arg, x)
        INTEGER, INTENT(IN), CONTIGUOUS :: Arg(:,:,:)
        INTEGER, INTENT(OUT) :: x

        x = SUM(Arg)
      END SUBROUTINE inner

END PROGRAM Nested1
