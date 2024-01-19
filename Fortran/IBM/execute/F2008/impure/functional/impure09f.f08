!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2012-04-16
!*
!*  DESCRIPTION
!*
!*  checks if an IMPURE elemental procedure works with File Handling using read, write,
!*  open and close statements!
!*  Also uses the CHARACTER data type inside the IMPURE elemental procedure.
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789



PROGRAM main
  IMPLICIT NONE

REAL :: a
a = 0.0
call writing (a)
call reading (a)

  CONTAINS

  IMPURE ELEMENTAL subroutine  writing (z)
    REAL,INTENT(IN)::Z

    REAL :: i
    OPEN (7, FILE = 'DummyData.txt' ,  STATUS = 'NEW')
    DO 10 i=1 , 100
    WRITE (7,*) I
 10   CONTINUE
   CLOSE (7)
  END subroutine writing

IMPURE ELEMENTAL subroutine reading (y)
   REAL,INTENT(INOUT) :: y
   REAL :: i
   CHARACTER(LEN=500) :: output
  OPEN (7, FILE = 'DummyData.txt')
   DO 10 i=2,50
    READ(7,*) output
      print*, output
    10    CONTINUE

    CLOSE (7)

  END subroutine reading



END PROGRAM main
