!*  ===================================================================
!*
!*  DATE                       : January 20, 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : ALLOCATE Statement with type-spec
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : Deferred LEN parameter
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!* allocate-stmt is
!*   ALLOCATE ( [ type-spec :: ] allocation-list [, alloc-opt-list ] )
!*
!* Defect 361318
!234567890123456789012345678901234567890123456789012345678901234567890
MODULE Mod
      IMPLICIT NONE

      TYPE Base  (k1,l1)
        INTEGER, KIND :: k1 = KIND(0)
        INTEGER, LEN  :: l1 = 10

        INTEGER(k1), ALLOCATABLE :: my_arr(:)
      END TYPE Base

      TYPE, EXTENDS(Base) :: Child (k2,l2)
        INTEGER, KIND :: k2 = KIND(0)
        INTEGER, LEN  :: l2 = 10

        CLASS(Base(k2,l1+l2)), POINTER :: b_cmp
      END TYPE Child
END MODULE Mod
PROGRAM AllocateWithTypeSpec01
      USE Mod
      IMPLICIT NONE

      INTEGER :: i, j, k, stat
      CHARACTER(100) :: errmsg

      TYPE(Child(4,:,4,:)), ALLOCATABLE :: c1

      IF ( ALLOCATED(c1)) ERROR STOP 10

      ALLOCATE(Child(4,100,4,100):: c1, STAT=stat, ERRMSG=errmsg)
      IF (stat .NE. 0) ERROR STOP 11
      IF (c1%l1 .NE. 100) ERROR STOP 12
      IF (c1%l2 .NE. 100) ERROR STOP 13

      IF ( ALLOCATED(c1%my_arr)) ERROR STOP 14

      ALLOCATE(c1%my_arr(c1%l1), SOURCE=(/(i, i = 1, c1%l1)/), STAT=stat, ERRMSG=errmsg)
      IF (stat .NE. 0) ERROR STOP 15
      IF (size(c1%my_arr) .NE. c1%l1 ) ERROR STOP 16

      IF (ASSOCIATED(c1%b_cmp)) ERROR STOP 17

      ALLOCATE(Base(4,(c1%l1+c1%l2)):: c1%b_cmp, STAT=stat, ERRMSG=errmsg)
      IF (stat .NE. 0) ERROR STOP 18
      IF (c1%b_cmp%l1 .NE. (c1%l1+c1%l2)) ERROR STOP 19

      IF ( ALLOCATED(c1%b_cmp%my_arr)) ERROR STOP 20

      ALLOCATE(c1%b_cmp%my_arr(c1%b_cmp%l1), SOURCE=(/(i, i = 1, c1%b_cmp%l1)/), STAT=stat, ERRMSG=errmsg)
      IF (stat .NE. 0) ERROR STOP 21
      IF (size(c1%b_cmp%my_arr) .NE. c1%b_cmp%l1 ) ERROR STOP 22

END PROGRAM AllocateWithTypeSpec01
