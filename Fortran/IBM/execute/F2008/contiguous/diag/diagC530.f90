! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-11-25
!*  ORIGIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   : CONTIGUOUS Attribute
!*  SECONDARY FUNCTIONS TESTED : -
!*                               -
!*
!*  DESCRIPTION                : -
!*                               -
!*  C530:
!*     An entity with the CONTIGUOUS attribute shall be an array POINTER
!*     or an assumed-shape array
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

      INTEGER, PARAMETER :: N = 1024

!* valid, rank = 1
      REAL(16), POINTER, CONTIGUOUS :: p1(:)
      CLASS(*), POINTER, CONTIGUOUS :: p2(:)
      REAL(8),  POINTER, DIMENSION(:), CONTIGUOUS :: p3 => NULL()

      REAL(8), POINTER :: p4(:)
      CONTIGUOUS p4

      CONTIGUOUS p5
      REAL(4), POINTER :: p5(:)

      CONTIGUOUS p6
      CLASS(*),  POINTER, DIMENSION(:) :: p6

      CLASS(*),  POINTER, DIMENSION(:) :: p7
      CONTIGUOUS p7

      CONTIGUOUS p8
      CLASS(*), DIMENSION(:) :: p8
      POINTER :: p8

      CLASS(*), DIMENSION(:) :: p9
      POINTER :: p9
      CONTIGUOUS p9

      CONTIGUOUS p10
      POINTER :: p10
      CLASS(*), DIMENSION(:), SAVE :: p10

      CLASS(*) :: p11(:)
      CONTIGUOUS p11
      POINTER :: p11

!* valid, rank = 3
      REAL(16), POINTER, CONTIGUOUS :: c1(:,:,:)
      CLASS(*), POINTER, CONTIGUOUS :: c2(:,:,:)
      REAL(8),  POINTER, DIMENSION(:,:,:), CONTIGUOUS :: c3 => NULL()

      REAL(8), POINTER :: c4(:,:,:)
      CONTIGUOUS c4

      CONTIGUOUS c5
      REAL(4), POINTER :: c5(:,:,:)

      CONTIGUOUS c6
      CLASS(*),  POINTER, DIMENSION(:,:,:) :: c6

      CLASS(*),  POINTER, DIMENSION(:,:,:) :: c7
      CONTIGUOUS c7

      CONTIGUOUS c8
      CLASS(*), DIMENSION(:,:,:) :: c8
      POINTER :: c8

      CLASS(*), DIMENSION(:,:,:) :: c9
      POINTER :: c9
      CONTIGUOUS c9

      CONTIGUOUS c10
      POINTER :: c10
      CLASS(*), DIMENSION(:,:,:), SAVE :: c10

      CLASS(*) :: c11(:,:,:)
      CONTIGUOUS c11
      POINTER :: c11

!*******************************************
!*    invalid, rank = 1
      REAL(16), CONTIGUOUS  :: t1(10)
      REAL(16), TARGET, CONTIGUOUS  :: t2(N)
      INTEGER, TARGET, DIMENSION(N), CONTIGUOUS :: t3

!*    invalid, rank = 5
      REAL(16), TARGET, CONTIGUOUS   :: t4(1,1,1,1,1)
      INTEGER, ALLOCATABLE, DIMENSION(:,:,:,:,:), CONTIGUOUS :: t5

      CONTAINS

      SUBROUTINE Sub1(J,Arg)
        INTEGER :: J
        INTEGER, POINTER, CONTIGUOUS :: ptr(:)
        INTEGER, TARGET, CONTIGUOUS :: Arg(:)
        INTEGER, CONTIGUOUS :: Obj(J)

      END SUBROUTINE Sub1

END MODULE
PROGRAM diagC530
      USE Mod
      IMPLICIT NONE
END PROGRAM diagC530
