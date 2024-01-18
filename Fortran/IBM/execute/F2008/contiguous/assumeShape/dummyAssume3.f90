! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-10-25
!*
!*  PRIMARY FUNCTIONS TESTED   : Data pointer assingment
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : - Target is dummy argument associated with
!*                                 contiguous actal argument
!*
!*    Dummy is assumed shape array with CONTIGUOUS attribute
!*    Actual is simply contiguous array pointer
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
PROGRAM dummyAssume3
      INTEGER, TARGET  :: I3D(2,2,2)
      INTEGER, POINTER, CONTIGUOUS  :: ptrc(:,:,:)

      I3D =  RESHAPE(SOURCE = [(I, I=1,8)], SHAPE = [2,2,2])

      ptrc => I3D
      CALL Sub(ptrc)
      IF ( ANY(ptrc .NE. RESHAPE( SOURCE = [1,2,2,4,2,4,4,8],      &
&                       SHAPE = [2,2,2])) )    ERROR STOP 10

      ptrc => I3D(:,:,:)
      CALL Sub(ptrc)
      IF ( ANY(ptrc .NE. RESHAPE( SOURCE = [1,2,2,4,2,4,4,8],      &
&                       SHAPE = [2,2,2])) )    ERROR STOP 10

      ptrc => I3D(:,:,1:)
      CALL Sub(ptrc)
      IF ( ANY(ptrc .NE. RESHAPE( SOURCE = [1,2,2,4,2,4,4,8],      &
&                       SHAPE = [2,2,2])) )    ERROR STOP 10

      ptrc => I3D(1:,1:,:)
      CALL Sub(ptrc)
      IF ( ANY(ptrc .NE. RESHAPE( SOURCE = [1,2,2,4,2,4,4,8],      &
&                       SHAPE = [2,2,2])) )    ERROR STOP 10

      ptrc => I3D(1:,1:,1:)
      CALL Sub(ptrc)
      IF ( ANY(ptrc .NE. RESHAPE( SOURCE = [1,2,2,4,2,4,4,8],      &
&                       SHAPE = [2,2,2])) )    ERROR STOP 10

      ptrc => I3D(1:2,1:2,:)
      CALL Sub(ptrc)
      IF ( ANY(ptrc .NE. RESHAPE( SOURCE = [1,2,2,4,2,4,4,8],      &
&                       SHAPE = [2,2,2])) )    ERROR STOP 10

      ptrc => I3D(1:2,1:2,1:2)
      CALL Sub(ptrc)
      IF ( ANY(ptrc .NE. RESHAPE( SOURCE = [1,2,2,4,2,4,4,8],      &
&                       SHAPE = [2,2,2])) )    ERROR STOP 10

      ptrc => I3D(:,:,1:)
      CALL Sub(ptrc)
      IF ( ANY(ptrc .NE. RESHAPE( SOURCE = [1,2,2,4,2,4,4,8],      &
&                       SHAPE = [2,2,2])) )    ERROR STOP 10

      ptrc => I3D(:,:,1:1)
      CALL Sub(ptrc)
      IF ( ANY(ptrc .NE. RESHAPE( SOURCE = [1,2,2,4,2,4,4,8],      &
&                       SHAPE = [2,2,2])) )    ERROR STOP 10

      ptrc => I3D(:,:,1:2)
      CALL Sub(ptrc)
      IF ( ANY(ptrc .NE. RESHAPE( SOURCE = [1,2,2,4,2,4,4,8],      &
&                       SHAPE = [2,2,2])) )    ERROR STOP 10

      ptrc => I3D(:,:,1:2:1)
      CALL Sub(ptrc)
      IF ( ANY(ptrc .NE. RESHAPE( SOURCE = [1,2,2,4,2,4,4,8],      &
&                       SHAPE = [2,2,2])) )    ERROR STOP 10

      ptrc => I3D(LBOUND(I3D, 1):UBOUND(I3D, 1),   &
                  LBOUND(I3D, 2):UBOUND(I3D, 2),   &
                  LBOUND(I3D, 3):UBOUND(I3D, 3))
      CALL Sub(ptrc)
      IF ( ANY(ptrc .NE. RESHAPE( SOURCE = [1,2,2,4,2,4,4,8],      &
&                       SHAPE = [2,2,2])) )    ERROR STOP 10

      CONTAINS

      SUBROUTINE Sub(Arg)
        INTEGER, TARGET, CONTIGUOUS :: Arg(:,:,:)
        INTEGER :: Itot, I1up, I1low, I2up, I2low, I3up, I3low

        Itot = size(Arg)

        I1up = UBOUND(Arg, 1)
        I2up = UBOUND(Arg, 2)
        I3up = UBOUND(Arg, 3)

        I1low = LBOUND(Arg, 1)
        I2low = LBOUND(Arg, 2)
        I3low = LBOUND(Arg, 3)

        IF ( .NOT. IS_CONTIGUOUS(Arg) ) ERROR STOP 11

        DO I = LBOUND(Arg, 1), UBOUND(Arg, 1), 1
           DO J = LBOUND(Arg, 2), UBOUND(Arg, 2), 1
              DO K = LBOUND(Arg, 3), UBOUND(Arg, 3), 1
                Arg(I,J,K) = I*J*K
              END DO
           END DO
        END DO

      END SUBROUTINE Sub
END PROGRAM dummyAssume3
