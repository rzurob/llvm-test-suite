! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-12-03
!*  ORIGIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   : CONTIGUOUS attribute
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : - test nesting with contiguous/not contiguous
!*                                actual and dummy argument
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
PROGRAM Nested4
      IMPLICIT NONE

      INTEGER, PARAMETER :: N = 4
      INTEGER :: I, I1(N), I2(2*N)
      INTEGER, TARGET :: tgt1(N)
      INTEGER, POINTER :: ptr1(:), ptr2(:)
      INTEGER, POINTER, CONTIGUOUS :: ptrc(:)
      LOGICAL :: Flag

      I1 = [(2*I,I=1,N)]
      I2 = [(-2*I,I=1,2*N)]
      tgt1 = I1 - 1
      ptr1 => tgt1
      ptrc => tgt1

      ALLOCATE( ptr2(N), SOURCE = [(-2*I+1,I=1,N)] )

! Calls to Sub_explicit_target: Dummy argument is explicit shape with TARGET attribute
! actual is explicit shape array

      Flag = IS_CONTIGUOUS(I1)
      CALL Sub_explicit_target(I1,N,flag)
      IF ( ANY(I1 .NE. [(2*I, I=1,N)]) )              ERROR STOP 10

      Flag = IS_CONTIGUOUS(I2(1:N))
      CALL Sub_explicit_target(I2(1:N),N,flag)
      IF ( ANY(I2 .NE. [(-2*I, I=1,2*N)]) )           ERROR STOP 11

      Flag = IS_CONTIGUOUS(I2(1:2*N:1))
      CALL Sub_explicit_target(I2(1:2*N:1),2*N,flag)
      IF ( ANY(I2 .NE. [(-2*I, I=1,2*N)]) )           ERROR STOP 12

! Actual is explicit shape array with TARGET attribute

      Flag = IS_CONTIGUOUS(tgt1)
      CALL Sub_explicit_target(tgt1,N,flag)
      IF ( ANY(tgt1 .NE. [(2*I-1, I=1,N)]) )          ERROR STOP 13

! Actual is associated array POINTER

      Flag = IS_CONTIGUOUS(ptr1)
      CALL Sub_explicit_target(ptr1,N,flag)
      IF ( ANY(ptr1 .NE. [(2*I-1, I=1,N)]) )          ERROR STOP 14

      Flag = IS_CONTIGUOUS(ptrc(1:N:1))
      CALL Sub_explicit_target(ptrc(1:N:1),N,flag)
      IF ( ANY(ptrc .NE. [(2*I-1, I=1,N)]) )          ERROR STOP 15

! Actual is allocated array POINTER

      Flag = IS_CONTIGUOUS(ptr2)
      CALL Sub_explicit_target(ptr2,N,flag)
      IF ( ANY(ptr2 .NE. [(-2*I+1, I=1,N)]) )         ERROR STOP 16

! Calls to Sub_assumed_target: Dummy argument is assumed shape with TARGET and CONTIGUOUS attribute
! actual is explicit shape array

      Flag = IS_CONTIGUOUS(I1)
      CALL Sub_assumed_target(I1,flag)
      IF ( ANY(I1 .NE. [(2*I, I=1,N)]) )              ERROR STOP 17

      Flag = IS_CONTIGUOUS(I2(1:N))
      CALL Sub_assumed_target(I2(1:N),flag)
      IF ( ANY(I2 .NE. [(-2*I, I=1,2*N)]) )           ERROR STOP 18

      Flag = IS_CONTIGUOUS(I2(1:2*N:1))
      CALL Sub_assumed_target(I2(1:2*N:1),flag)
      IF ( ANY(I2 .NE. [(-2*I, I=1,2*N)]) )           ERROR STOP 19

! Actual is explicit shape array with TARGET attribute

      Flag = IS_CONTIGUOUS(tgt1)
      CALL Sub_assumed_target(tgt1,flag)
      IF ( ANY(tgt1 .NE. [(2*I-1, I=1,N)]) )          ERROR STOP 20

! Actual is associated array pointer

      Flag = IS_CONTIGUOUS(ptr1)
      CALL Sub_assumed_target(ptr1,flag)
      IF ( ANY(ptr1 .NE. [(2*I-1, I=1,N)]) )          ERROR STOP 21

      Flag = IS_CONTIGUOUS(ptrc(1:N:1))
      CALL Sub_assumed_target(ptrc(1:N:1),flag)
      IF ( ANY(ptrc .NE. [(2*I-1, I=1,N)]) )          ERROR STOP 22

! Actual is allocated array pointer

      Flag = IS_CONTIGUOUS(ptr2)
      CALL Sub_assumed_target(ptr2,flag)
      IF ( ANY(ptr2 .NE. [(-2*I+1, I=1,N)]) )         ERROR STOP 23

! Calls to Sub_pointer: Dummy argument CONTIGUOUS pointer
! actual is associated array pointer with CONTIGUOUS attribute

      Flag = IS_CONTIGUOUS(ptrc)
      CALL Sub_pointer(ptrc,flag)
      IF ( ANY(ptrc .NE. [(2*I-1, I=1,N)]) )          ERROR STOP 24

      DEALLOCATE(ptr2)

      CONTAINS

      SUBROUTINE Sub_explicit_target(Arg,N,test)
         INTEGER, INTENT(IN), TARGET :: Arg(N)
         INTEGER, INTENT(IN) :: N
         INTEGER, POINTER :: pObj(:)
         LOGICAL :: test

         pObj => Arg
         IF ( IS_CONTIGUOUS(Arg) .NEQV. .true. )  ERROR STOP 30
      END SUBROUTINE Sub_explicit_target

      SUBROUTINE Sub_assumed_target(Arg,test1)
         INTEGER, INTENT(IN), TARGET, CONTIGUOUS :: Arg(:)
         INTEGER, POINTER :: pObj(:)
         INTEGER :: N
         LOGICAL :: test1, test2

         N = Size(Arg)
         pObj => Arg
         test2 = IS_CONTIGUOUS(Arg)
         IF ( test1 .NEQV. test2 )       ERROR STOP 31
         CALL Sub_explicit_target(Arg,N,test2)
      END SUBROUTINE Sub_assumed_target

      SUBROUTINE Sub_pointer(Arg,test1)
         INTEGER, POINTER, CONTIGUOUS :: Arg(:)
         INTEGER, POINTER :: pObj(:)
         INTEGER :: N
         LOGICAL :: test1, test2

         N = Size(Arg)
         pObj => Arg
         test2 = IS_CONTIGUOUS(Arg)
         IF ( test1 .NEQV. test2 )       ERROR STOP 32
         CALL Sub_explicit_target(Arg,N,test2)
      END SUBROUTINE Sub_pointer

END PROGRAM Nested4
