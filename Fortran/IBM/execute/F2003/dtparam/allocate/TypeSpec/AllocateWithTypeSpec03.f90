!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : AllocateWithTypeSpec03 
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
!* Defect 361391
!234567890123456789012345678901234567890123456789012345678901234567890
MODULE Mod
      IMPLICIT NONE 

      TYPE Base  (k1,l1)
        INTEGER, KIND :: k1 = KIND(0)
        INTEGER, LEN  :: l1 = 10
      END TYPE Base

      TYPE, EXTENDS(Base) :: Child (k2,l2)
        INTEGER, KIND :: k2 = KIND(0)
        INTEGER, LEN  :: l2 = 10

        INTEGER(k1+k2), ALLOCATABLE :: my_arr(:)
        CLASS(Base(k2,l1+l2)), POINTER :: b_cmp
      END TYPE Child

      TYPE Branch  (q3,n1,n2,n3)
        INTEGER, KIND :: q3 = KIND(0)
        INTEGER, LEN  :: n1 = 2, n2 = 2, n3 = 2 

        TYPE(Child(q3,n2-n1,q3,n2)) :: c_cmp(n3)
      END TYPE Branch
END MODULE Mod
PROGRAM AllocateWithTypeSpec03
      USE Mod
      IMPLICIT NONE 

      INTEGER(8) :: i
      INTEGER :: j, k, stat
      CHARACTER(100) :: errmsg

      TYPE(Child), ALLOCATABLE :: c1        !<---- this is allowed by the F2003 standard (C478)
      CLASS(Branch(4,:,:,:)), ALLOCATABLE :: b1(:,:) 

      IF ( ALLOCATED(c1)) STOP 10

      ALLOCATE(Child :: c1, STAT=stat, ERRMSG=errmsg)
      IF (stat .NE. 0) STOP 11
      IF (c1%l1 .NE. 10) STOP 12
      IF (c1%l2 .NE. 10) STOP 13

      IF ( ALLOCATED(c1%my_arr)) STOP 14

      ALLOCATE(c1%my_arr(c1%l1), SOURCE=(/(i, i = 1, c1%l1)/), STAT=stat, ERRMSG=errmsg) 
      IF (stat .NE. 0) STOP 15
      IF (size(c1%my_arr) .NE. c1%l1 ) STOP 16

      IF (ASSOCIATED(c1%b_cmp)) STOP 17

      ALLOCATE(Base(4,(c1%l1+c1%l2)):: c1%b_cmp, STAT=stat, ERRMSG=errmsg)
      IF (stat .NE. 0) STOP 18
      IF (c1%b_cmp%l1 .NE. (c1%l1+c1%l2)) STOP 19

      IF ( ALLOCATED(b1)) STOP 20

      ALLOCATE(Branch(4,2,5,10):: b1(3,3), STAT=stat, ERRMSG=errmsg)
      IF (stat .NE. 0) STOP 21
      IF (b1%n1 .NE. 2) STOP 22
      IF (b1%n2 .NE. 5) STOP 23
      IF (b1%n3 .NE. 10) STOP 24

      DO i = 1, 3 
          DO j = 1, 3 
             IF ( size(b1(i,j)%c_cmp) .NE. 10) STOP 25
             DO k = 1, 10
                ALLOCATE(Base(4,8) :: b1(i,j)%c_cmp(k)%b_cmp, STAT=stat, ERRMSG=errmsg)
                IF (stat .NE. 0) STOP 26
                IF (b1(i,j)%c_cmp(k)%b_cmp%l1 .NE. 8) STOP 27 

             END DO 
          END DO 
      END DO 

END PROGRAM AllocateWithTypeSpec03
