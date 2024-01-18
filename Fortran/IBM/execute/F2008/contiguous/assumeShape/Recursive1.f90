! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-10-18
!*
!*  PRIMARY FUNCTIONS TESTED   : CONTIGUOUS attribute
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : - Recursive subroutine call with assumed
!*                                   shape array where the shape changes at
!*                                   each recursive call
!*                               - Actual argument is simply contiguous
!*                               - Dummy argument is an assumed shape array with
!*                                   CONTIGUOUS attribute
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
PROGRAM Recursive1
      IMPLICIT NONE

      INTEGER :: i, matrix(5,5)

      DO i = 1, 5
          matrix(:,i) = [10,5,3,4,6]
      ENDDO

      call SUB1( matrix )
      DO i = 1, 5
          IF ( ANY(matrix(:,i) .NE. [3,4,10,5,6]) ) STOP 10
      ENDDO


      CONTAINS

      RECURSIVE SUBROUTINE SUB1( Arg1 )
         INTEGER, CONTIGUOUS :: Arg1(:,:)
         INTEGER :: i1, i2, i3
         INTEGER :: START, STOP, LOW, HIGH

         IF ( UBOUND(Arg1,1) .LE. LBOUND(Arg1,1) ) RETURN

         DO i1 = LBOUND(Arg1,2),UBOUND(Arg1,2)
            START = LBOUND(Arg1,1)
            STOP  = UBOUND(Arg1,1)
            LOW = Arg1(START,i1)
            HIGH = LOW

            DO i2 = START+1, STOP
               i3 = Arg1(i2,i1)

               IF (i3 .LT. LOW) THEN
                  Arg1(START,i1) = Arg1(i2,i1)
                  Arg1(i2,i1) = LOW
                  LOW = Arg1(START,i1)
               ENDIF

               IF (i3 .GT. HIGH) THEN
                  Arg1(START,i1) = Arg1(i2,i1)
                  Arg1(i2,i1) = HIGH
                  HIGH = Arg1(START,i1)
               ENDIF

            ENDDO
         ENDDO
         CALL SUB1( Arg1(START+1:STOP-1,:) )

      END SUBROUTINE SUB1

END PROGRAM Recursive1
