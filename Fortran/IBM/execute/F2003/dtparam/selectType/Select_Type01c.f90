!*  ===================================================================
!*
!*                               DTP-SELECT TYPE Construct
!*
!*  DATE                       : August  12, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : SELECT TYPE Construct - Derived-type parameters
!*  SECONDARY FUNCTIONS TESTED : Allocation of a pointer inside the SELECT TYPE Construct
!*                               Host association/Argument association
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : SELECT TYPE Construct
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*  8.1.5.1 Form of the SELECT TYPE construct
!*
!*  R821 select-type-construct  is  select-type-stmt
!*                                      [ type-guard-stmt
!*                                        block ] ...
!*                                      end-select-type-stmt
!*  R822 select-type-stmt       is  [ select-construct-name : ] SELECT TYPE&
!*                                      &( [ associate-name => ] selector )
!*
!*  R823 type-guard-stmt is TYPE IS ( type-spec ) [ select-construct-name ]
!*                       or CLASS IS ( type-spec ) [ select-construct-name ]
!*                       or CLASS DEFAULT [ select-construct-name ]
!*
!* See defect 355488
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      PROGRAM Select_Type01c
      IMPLICIT NONE
!*
! DERIVED TYPE declarations
!*
      TYPE Base  (k1,len1)
        INTEGER, KIND :: k1 = KIND(0)
        INTEGER, LEN :: len1 = 20

        INTEGER(KIND=k1) :: my_arr(len1)
      END TYPE Base

      TYPE, EXTENDS(Base) :: Child
        CLASS(Base(k1,len1)), POINTER :: Cmp
      END TYPE Child

      INTEGER, PARAMETER :: k1 = KIND(0), len1 = 10
      INTEGER :: I
      CLASS(*), ALLOCATABLE :: child1
      TYPE(Base(k1,len1)), TARGET :: tgt

      tgt%my_arr=(/ (I, I = 1, len1) /)

      ALLOCATE(Child(k1,len1):: child1)
      IF ( .NOT. ALLOCATED(child1)) STOP 10

      CALL sub2

      CONTAINS
!*
      SUBROUTINE sub2

      SELECT TYPE (A => child1)
        CLASS IS (Child(k1,*))
           !ALLOCATE(Base(k1,len1):: A%Cmp) ! passes if this line is added
           A%Cmp => tgt
           IF ( .NOT. ASSOCIATED(A%Cmp)) STOP 20
           CALL sub1(A%Cmp)

        CLASS IS (Base(k1,*))
           STOP 21

        CLASS DEFAULT
           STOP 22
      END SELECT

      END SUBROUTINE sub2
!*
      SUBROUTINE sub1(Obj)

      CLASS(Base(k1,len1)), POINTER :: Obj

      SELECT TYPE (A => Obj)
        TYPE IS (Child(k1,*))
           STOP 11

        TYPE IS (Base(k1,*))
          IF (SIZE(A%my_arr) .NE. len1) STOP 12

        CLASS DEFAULT
           STOP 13
      END SELECT

      END SUBROUTINE sub1
!*
      END PROGRAM Select_Type01c
