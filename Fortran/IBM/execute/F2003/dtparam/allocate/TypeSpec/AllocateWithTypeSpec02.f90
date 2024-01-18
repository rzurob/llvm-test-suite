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
!234567890123456789012345678901234567890123456789012345678901234567890
MODULE Mod
      IMPLICIT NONE

      TYPE Base  (k1,l1)
        INTEGER, KIND :: k1 = KIND(0)
        INTEGER, LEN  :: l1 = 2
      END TYPE Base

      TYPE, EXTENDS(Base) :: Child (k2,l2)
        INTEGER, KIND :: k2 = KIND(0)
        INTEGER, LEN  :: l2 = 5

        INTEGER(k1+k2), ALLOCATABLE :: my_arr(:)
        CLASS(Base(k2,l1+l2)), POINTER :: b_cmp
      END TYPE Child
END MODULE Mod
PROGRAM AllocateWithTypeSpec02
      USE Mod
      IMPLICIT NONE

      INTEGER(8) :: i
      INTEGER(4) :: stat
      CHARACTER(100) :: errmsg

      TYPE(Child), ALLOCATABLE :: c1        !<---- this is allowed by the F2003 standard (C478)

      IF ( ALLOCATED(c1)) ERROR STOP 10

      ALLOCATE(Child :: c1, STAT=stat, ERRMSG=errmsg)
      IF (stat .NE. 0) ERROR STOP 11
      IF (c1%l1 .NE. 2) ERROR STOP 12
      IF (c1%l2 .NE. 5) ERROR STOP 13

      IF ( ALLOCATED(c1%my_arr)) ERROR STOP 14

      ALLOCATE(c1%my_arr(c1%l1), SOURCE=(/(i, i = 1, c1%l1)/), STAT=stat, ERRMSG=errmsg)
      IF (stat .NE. 0) ERROR STOP 15
      IF (size(c1%my_arr) .NE. c1%l1 ) ERROR STOP 16

      IF (ASSOCIATED(c1%b_cmp)) ERROR STOP 17

      ALLOCATE(Child(4,(c1%l1+c1%l2),4,c1%l2):: c1%b_cmp, STAT=stat, ERRMSG=errmsg)
      IF (stat .NE. 0) ERROR STOP 18

      SELECT TYPE ( A => c1%b_cmp)
        TYPE IS (Child(4,*,4,*))
           IF (A%l1 .NE. (c1%l1+c1%l2)) ERROR STOP 19
           IF (A%l2 .NE. c1%l2) ERROR STOP 20

           IF ( ALLOCATED(A%my_arr)) ERROR STOP 21

           ALLOCATE(A%my_arr(A%l1), SOURCE=(/(i, i = 1, A%l1)/), STAT=stat, ERRMSG=errmsg)
           IF (stat .NE. 0) ERROR STOP 22
           IF (size(A%my_arr) .NE. A%l1 ) ERROR STOP 23

           IF (ASSOCIATED(A%b_cmp)) ERROR STOP 24

           ALLOCATE(Base(4,(A%l1+A%l2)):: A%b_cmp, STAT=stat, ERRMSG=errmsg)
           IF (stat .NE. 0) ERROR STOP 25
           IF (A%b_cmp%l1 .NE. (A%l1+A%l2)) ERROR STOP 26

        CLASS DEFAULT
           STOP 27
      END SELECT

END PROGRAM AllocateWithTypeSpec02
