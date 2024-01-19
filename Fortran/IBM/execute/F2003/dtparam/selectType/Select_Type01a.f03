!*  ===================================================================
!*
!*                               DTP-SELECT TYPE Construct
!*
!*  DATE                       : August 11, 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : SELECT TYPE Construct - Derived-type parameters
!*  SECONDARY FUNCTIONS TESTED : ASSOCIATE Construct inside a SELECT TYPE Construct
!*                               Host association -  Array Constructor
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
      PROGRAM Select_Type01a
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
      INTEGER :: I, J
      TYPE(Child(k1,len1)) :: child1

      ALLOCATE(Base(k1,len1):: child1%Cmp)
      IF ( .NOT. ASSOCIATED(child1%Cmp)) ERROR STOP 10

      CALL sub1

      CALL sub2

      CONTAINS
!*
      SUBROUTINE sub1

      SELECT TYPE ( A => child1%Cmp)
        TYPE IS (Child(k1,*))
           STOP 10

        TYPE IS (Base(k1,*))
          print*, SIZE(A%my_arr)
          print*, FACT(SIZE(A%my_arr))

        CLASS DEFAULT
           STOP 11
      END SELECT

      END SUBROUTINE sub1
!*
      SUBROUTINE sub2

      SELECT TYPE ( A => child1%Cmp)
        TYPE IS (Child(k1,*))
           STOP 20

        CLASS IS (Child(k1,*))
           STOP 21

        CLASS IS (Base(k1,*))
           ASSOCIATE ( p => A%my_arr )
               p = (/ (I, I = 1, SIZE(p)) /)
               print*, SUM(p)
           END ASSOCIATE

        CLASS DEFAULT
           STOP 22
      END SELECT

      END SUBROUTINE sub2
!*
      INTEGER FUNCTION FACT(N)
        INTEGER :: K, N

        FACT = PRODUCT ((/ (K, K = 2, N) /))
      END FUNCTION

      END PROGRAM Select_Type01a
