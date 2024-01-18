!*  ===================================================================
!*
!*                               DTP-SELECT TYPE Construct
!*
!*  DATE                       : August 11, 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : SELECT TYPE Construct - Derived-type parameters
!*  SECONDARY FUNCTIONS TESTED : SELECT TYPE Construct inside an ASSOCIATE Construct
!*                               Host association - Array Constructor
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
!234567890123456789012345678901234567890123456789012345678901234567890
      PROGRAM Select_Type01b
      IMPLICIT NONE
!*
! DERIVED TYPE declarations
!*
      TYPE Base  (k1,len1)
        INTEGER, KIND :: k1   !4
        INTEGER, LEN :: len1  !5

        INTEGER(KIND=k1) :: my_arr(len1)
      END TYPE Base

      TYPE, EXTENDS(Base) :: Child
        CLASS(Base(k1,len1)), ALLOCATABLE :: Cmp(:)
      END TYPE Child

      INTEGER, PARAMETER :: k1 = KIND(0), len1 = 5
      INTEGER :: I, J
      CLASS(Child(k1,len1)), POINTER :: child1
      TYPE(Child(k1,len1)), TARGET :: tgt

      ALLOCATE(Base(k1,len1):: tgt%Cmp(2))
      IF ( .NOT. ALLOCATED(tgt%Cmp)) STOP 10

      tgt%my_arr = 0

      child1 => tgt
      IF ( .NOT. ASSOCIATED(child1)) STOP 11
      IF ( .NOT. ALLOCATED(child1%Cmp)) STOP 12

      CALL sub1

      CALL sub2

      CONTAINS
!*
      SUBROUTINE sub1

      SELECT TYPE ( A => child1%Cmp(1))
        TYPE IS (Child(k1,*))
           STOP 20

        TYPE IS (Base(k1,*))
          print*, SIZE(A%my_arr)
          print*, FACT(SIZE(A%my_arr))

        CLASS DEFAULT
           STOP 21
      END SELECT

      END SUBROUTINE sub1
!*
      SUBROUTINE sub2

      ASSOCIATE (A => child1%Cmp(2))
          SELECT TYPE ( A )
            TYPE IS (Child(k1,*))
              STOP 30

          CLASS IS (Child(k1,*))
              STOP 31

          CLASS IS (Base(k1,*))
                  A%my_arr = (/ (I, I = 1, SIZE(A%my_arr)) /)
                  print*, SUM(A%my_arr)

          CLASS DEFAULT
             STOP 32
      END SELECT

      END ASSOCIATE

      END SUBROUTINE sub2
!*
      INTEGER FUNCTION FACT(N)
        INTEGER :: K, N

        FACT = PRODUCT ((/ (K, K = 2, N) /))
      END FUNCTION

      END PROGRAM Select_Type01b
