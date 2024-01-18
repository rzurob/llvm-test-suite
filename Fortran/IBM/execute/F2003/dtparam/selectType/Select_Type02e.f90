!*  ===================================================================
!*
!*                               DTP-SELECT TYPE Construct
!*
!*  DATE                       : August 25, 2008
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
      MODULE Mod1
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
        CLASS(*), ALLOCATABLE :: Cmp(:)
      END TYPE Child

      TYPE, EXTENDS(Child) :: NextGen
      END TYPE NextGen

      INTEGER, PARAMETER :: k1 = KIND(0), len1 = 5

      CONTAINS

      INTEGER FUNCTION FACT(N)
        INTEGER :: K, N
        FACT = PRODUCT ((/ (K, K = 2, N) /))
      END FUNCTION

      END MODULE Mod1

      PROGRAM Select_Type02e
      USE Mod1
      IMPLICIT NONE

      CLASS(Child(k1,len1)), POINTER :: child1
      TYPE(Child(k1,len1)), TARGET :: tgt
      TYPE(NextGen(k1,len1)), TARGET :: cbl

      INTERFACE
        SUBROUTINE sub1(Obj)
          USE Mod1
          IMPLICIT NONE
          CLASS(Child(k1,len1)) :: Obj
        END SUBROUTINE sub1
!*
        SUBROUTINE sub2(Obj)
          USE Mod1
          IMPLICIT NONE
          CLASS(NextGen(k1,len1)) :: Obj
          INTEGER :: I
        END SUBROUTINE sub2
      END INTERFACE

      ALLOCATE(Base(k1,len1):: tgt%Cmp(2))
      ALLOCATE(Base(k1,len1):: cbl%Cmp(len1))
      IF ( .NOT. ALLOCATED(tgt%Cmp)) STOP 100
      IF ( .NOT. ALLOCATED(cbl%Cmp)) STOP 101

      child1 => tgt
      IF ( .NOT. ASSOCIATED(child1)) STOP 102
      IF ( .NOT. ALLOCATED(child1%Cmp)) STOP 103

      CALL sub1(child1)

      !dynamic TYPE of child is now NextGen
      child1 => cbl
      IF ( .NOT. ASSOCIATED(child1)) STOP 104
      IF ( .NOT. ALLOCATED(child1%Cmp)) STOP 105

      SELECT TYPE (child1)
         CLASS IS (NextGen(k1,*))
            !call possible only within the SELECT TYPE
            CALL sub2(child1)

         CLASS DEFAULT
            STOP 10
        END SELECT

      END PROGRAM Select_Type02e
!*
      SUBROUTINE sub1(Obj)
        USE Mod1
        IMPLICIT NONE
        CLASS(Child(k1,len1)) :: Obj

        SELECT TYPE ( A => Obj%Cmp(1))
          TYPE IS (Child(k1,*))
            STOP 20

          TYPE IS (Base(k1,*))
            IF( SIZE(A%my_arr) /= len1) STOP 111
            IF( FACT(SIZE(A%my_arr)) /= FACT(len1) ) STOP 112

          CLASS DEFAULT
            STOP 21
        END SELECT

      END SUBROUTINE sub1
!*
      SUBROUTINE sub2(Obj)
        USE Mod1
        IMPLICIT NONE
        CLASS(NextGen(k1,len1)) :: Obj
        INTEGER :: I

        ASSOCIATE (A => (/Obj%Cmp/) )
          SELECT TYPE ( A )
            TYPE IS (Child(k1,*))
              STOP 30

          CLASS IS (Child(k1,*))
              STOP 31

          CLASS IS (Base(k1,*))
              IF( SIZE(A) /= len1) STOP 111

          CLASS DEFAULT
             STOP 32
          END SELECT

        END ASSOCIATE

      END SUBROUTINE sub2
