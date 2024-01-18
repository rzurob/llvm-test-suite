!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : AllocateWithTypeSpec02 
!*
!*  PROGRAMMER                 : Dorra Bouchiha 
!*  DATE                       : January 20, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : ALLOCATE Statement with type-spec
!*  SECONDARY FUNCTIONS TESTED :
!*                               
!*
!*  DRIVER STANZA              : xlf2003
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

      IF ( ALLOCATED(c1)) STOP 10

      ALLOCATE(Child :: c1, STAT=stat, ERRMSG=errmsg)
      IF (stat .NE. 0) STOP 11
      IF (c1%l1 .NE. 2) STOP 12
      IF (c1%l2 .NE. 5) STOP 13

      IF ( ALLOCATED(c1%my_arr)) STOP 14

      ALLOCATE(c1%my_arr(c1%l1), SOURCE=(/(i, i = 1, c1%l1)/), STAT=stat, ERRMSG=errmsg) 
      IF (stat .NE. 0) STOP 15
      IF (size(c1%my_arr) .NE. c1%l1 ) STOP 16

      IF (ASSOCIATED(c1%b_cmp)) STOP 17

      ALLOCATE(Child(4,(c1%l1+c1%l2),4,c1%l2):: c1%b_cmp, STAT=stat, ERRMSG=errmsg)
      IF (stat .NE. 0) STOP 18

      SELECT TYPE ( A => c1%b_cmp)
        TYPE IS (Child(4,*,4,*))
           IF (A%l1 .NE. (c1%l1+c1%l2)) STOP 19
           IF (A%l2 .NE. c1%l2) STOP 20

           IF ( ALLOCATED(A%my_arr)) STOP 21

           ALLOCATE(A%my_arr(A%l1), SOURCE=(/(i, i = 1, A%l1)/), STAT=stat, ERRMSG=errmsg)
           IF (stat .NE. 0) STOP 22
           IF (size(A%my_arr) .NE. A%l1 ) STOP 23

           IF (ASSOCIATED(A%b_cmp)) STOP 24

           ALLOCATE(Base(4,(A%l1+A%l2)):: A%b_cmp, STAT=stat, ERRMSG=errmsg)
           IF (stat .NE. 0) STOP 25
           IF (A%b_cmp%l1 .NE. (A%l1+A%l2)) STOP 26

        CLASS DEFAULT
           STOP 27
      END SELECT

END PROGRAM AllocateWithTypeSpec02
