! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-10-25
!*
!*  PRIMARY FUNCTIONS TESTED   : Data pointer assingment
!*  SECONDARY FUNCTIONS TESTED : Target is assumed-size array
!*
!*  DESCRIPTION                : -
!*                               -
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
PROGRAM assumeSizeTar
      IMPLICIT NONE

      INTEGER, TARGET               :: I1D(10)
      INTEGER, TARGET               :: I2D(5,5)
      INTEGER, ALLOCATABLE          :: iaa(:)

      CALL Sub(I1D,10)
      CALL Sub(I1D(5:),5)
      CALL Sub(I1D(1:0),0)
      CALL Sub(I1D(4:9),5)
      CALL Sub(I1D(2:10:1),9)
      CALL Sub(I1D(2:10:2),5)
      CALL Sub(I1D(5:1:-1),5)

      CALL Sub(I2D,25)
      CALL Sub(I2D(:5,:5),10)
      CALL Sub(I2D(:5,:),25)
      CALL Sub(I2D(:,1),5)
      CALL Sub(I2D(1,:),5)

      ALLOCATE(iaa(10))
      CALL Sub(iaa,10)
      CALL Sub(iaa(5:),5)
      CALL Sub(iaa(1:0),0)
      CALL Sub(iaa(4:9),5)
      CALL Sub(iaa(2:10:1),9)
      CALL Sub(iaa(1:10:2),5)

      CONTAINS

      SUBROUTINE Sub(Arg, DIM)
        INTEGER :: DIM
        INTEGER, TARGET :: Arg (*)
        INTEGER, POINTER, CONTIGUOUS  :: ptr(:)

        IF ( IS_CONTIGUOUS(Arg) ) THEN
           ptr => Arg(1:DIM)
        ELSE
           ptr => NULL()
        END IF
      END SUBROUTINE
END PROGRAM assumeSizeTar
