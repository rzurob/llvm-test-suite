! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : Nested1.f
!*
!*  PROGRAMMER                 : Dorra Bouchiha 
!*  DATE                       : 2010-10-25
!*  ORIGIN                     :
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Nested subroutine call
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              :
!*
!*  DESCRIPTION                : 
!*       
!*                      
!*    - Inner most subroutine has explit shape array dummy argument 
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

      INTEGER :: I, J, K, init
      INTEGER, TARGET  :: I3D(5,5,5)
      INTEGER, POINTER :: ptr(:,:,:)

      I3D = RESHAPE( SOURCE = [(I, I=1,125)], SHAPE = [5,5,5] )
      ptr => I3D 
      CALL Sub1(ptr)

      init = 1
      DO K = 1, 5
         DO J = 1, 5
           DO I = 1, 5
               IF ( ptr(I, J, K) .NE. I+J+K ) ERROR STOP 100
               ptr(I, J, K) = init 
               init = init + 1
           ENDDO
         ENDDO
       ENDDO

      CALL Sub2(ptr)

      init = 1
      DO K = 1, 5
         DO J = 1, 5
           DO I = 1, 5
               IF ( ptr(I, J, K) .NE. I+J+K ) ERROR STOP 101
               ptr(I, J, K) = init
               init = init + 1
           ENDDO
         ENDDO
       ENDDO

      ALLOCATE( ptr(5,5,5), SOURCE=I3D )
      CALL Sub1(ptr)

      init = 1
      DO K = 1, 5
         DO J = 1, 5
           DO I = 1, 5
               IF ( ptr(I, J, K) .NE. I+J+K ) ERROR STOP 102
               ptr(I, J, K) = init
               init = init + 1
           ENDDO
         ENDDO
       ENDDO

      CALL Sub2(ptr)

      init = 1
      DO K = 1, 5
         DO J = 1, 5
           DO I = 1, 5
               IF ( ptr(I, J, K) .NE. I+J+K ) ERROR STOP 103
               ptr(I, J, K) = init
               init = init + 1
           ENDDO
         ENDDO
       ENDDO

      CONTAINS

      SUBROUTINE Sub1(Arg)           
        INTEGER :: Arg(:,:,:)

        IF ( .NOT. IS_CONTIGUOUS(Arg) ) STOP 10
        IF ( ANY(Arg .NE. RESHAPE(SOURCE=[(I, I=1,125)], SHAPE=[5,5,5])) ) ERROR STOP 13

        CALL SubSub(Arg) 
        IF ( .NOT. IS_CONTIGUOUS(Arg) ) STOP 11

      END SUBROUTINE Sub1
      
      SUBROUTINE Sub2(Arg)
        INTEGER, POINTER :: Arg(:,:,:)

        IF ( .NOT. ASSOCIATED(Arg)    ) STOP 20
        IF ( .NOT. IS_CONTIGUOUS(Arg) ) STOP 21
        IF ( ANY(Arg .NE. RESHAPE(SOURCE=[(I, I=1,125)], SHAPE=[5,5,5])) ) ERROR STOP 22

        CALL SubSub(Arg)
        IF ( .NOT. ASSOCIATED(Arg)    ) STOP 23
        IF ( .NOT. IS_CONTIGUOUS(Arg) ) STOP 24

      END SUBROUTINE Sub2

      SUBROUTINE SubSub(Arg)
        INTEGER :: Arg(5,5,5)

        IF ( .NOT. IS_CONTIGUOUS(Arg) ) STOP 30
        CALL SubSubSub(Arg)

        DO K = 1, 5
          DO J = 1, 5
            DO I = 1, 5
                Arg(I, J, K) = I+J+K
            ENDDO
          ENDDO
        ENDDO
      END SUBROUTINE SubSub

      SUBROUTINE SubSubSub(Arg)
        INTEGER, CONTIGUOUS :: Arg(:,:,:)

        IF ( .NOT. IS_CONTIGUOUS(Arg) ) STOP 40

      END SUBROUTINE SubSubSub
END PROGRAM Nested1
