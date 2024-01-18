!*  ===================================================================
!*
!*                               DTP-SELECT TYPE Construct
!*
!*  DATE                       : August 25, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : SELECT TYPE Construct - Derived-type parameters
!*  SECONDARY FUNCTIONS TESTED : SELECT TYPE Construct inside an ASSOCIATE Construct
!*                               Use association - Array Constructor
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

      TYPE Base  (k1,l1)
        INTEGER, KIND :: k1  !4
        INTEGER, LEN  :: l1  !5
      END TYPE Base

      TYPE, EXTENDS(Base) :: Child
        CLASS(*), ALLOCATABLE :: Cmp(:)
      END TYPE Child

      TYPE, EXTENDS(Child) :: NextGen
      END TYPE NextGen

      INTEGER, PARAMETER :: knd1 = KIND(0), len1 = 5

      CONTAINS

      SUBROUTINE sub2(Obj)
        CLASS(NextGen(knd1,len1)) :: Obj

          SELECT TYPE ( A => (/Obj%Cmp/) )
            TYPE IS (Child(knd1,*))
              STOP 30

          CLASS IS (Child(knd1,*))
              STOP 31

          CLASS IS (Base(knd1,*))
              IF ( SIZE(A) /= len1 ) STOP 32

          CLASS DEFAULT
              STOP 33
          END SELECT
      END SUBROUTINE sub2
END MODULE Mod1
!*
PROGRAM Select_Type03c
      USE Mod1
      IMPLICIT NONE

      CLASS(Child(knd1,len1)), POINTER :: child1
      TYPE(NextGen(knd1,len1)), TARGET :: cbl

      ALLOCATE( Base(knd1,len1):: cbl%Cmp(len1) )
      IF ( .NOT. ALLOCATED(cbl%Cmp) ) STOP 10

      child1 => cbl                             !dynamic TYPE of child is NextGen
      IF ( .NOT. ASSOCIATED(child1)) STOP 11
      IF ( .NOT. ALLOCATED(child1%Cmp)) STOP 12

      SELECT TYPE ( child1 )
         CLASS IS (NextGen(knd1,*))
            !call possible only within the SELECT TYPE
            CALL sub2(child1)

         CLASS DEFAULT
            STOP 20
        END SELECT
END PROGRAM Select_Type03c
