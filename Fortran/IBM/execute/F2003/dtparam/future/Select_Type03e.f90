!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : Select_Type03c - SELECT TYPE 
!*                               DTP-SELECT TYPE Construct
!*
!*  PROGRAMMER                 : Dorra Bouchiha 
!*  DATE                       : August 25, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : SELECT TYPE Construct - Derived-type parameters
!*  SECONDARY FUNCTIONS TESTED : SELECT TYPE Construct inside an ASSOCIATE Construct
!*                               Host association - Array Constructor
!*                       
!*
!*  DRIVER STANZA              : xlf2003
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
      TYPE Base  (k1,l1)
        INTEGER, KIND :: k1   !4
        INTEGER, LEN :: l1  !5
      END TYPE Base 

      TYPE, EXTENDS(Base) :: Child
        CLASS(*), ALLOCATABLE :: Cmp(:) 
      END TYPE Child 

      TYPE, EXTENDS(Child) :: NextGen
      END TYPE NextGen

      INTEGER, PARAMETER :: knd1 = KIND(0), len1 = 5
  
      CONTAINS

      SUBROUTINE sub2(Obj)
        CLASS(NextGen(k1=knd1,l1=len1)) :: Obj
        INTEGER :: I

          SELECT TYPE ( A => (/ (Obj%Cmp, I = 1,len1) /) )
            TYPE IS (Child(knd1,*))
              STOP 30

          CLASS IS (Child(knd1,*))
              STOP 31

          CLASS IS (Base(knd1,*))
              IF( SIZE(A) /= len1*len1) STOP 111

          CLASS DEFAULT
             STOP 32
          END SELECT

      END SUBROUTINE sub2

      END MODULE Mod1

      PROGRAM Select_Type03c
      USE Mod1, Chd => Child, NtG => NextGen, foo => sub2
      IMPLICIT NONE 

      CLASS(Chd(knd1,len1)), POINTER :: child1 
      TYPE(NtG(knd1,len1)), TARGET :: cbl

      ALLOCATE(Base(knd1,len1):: cbl%Cmp(len1))
      IF ( .NOT. ALLOCATED(cbl%Cmp)) STOP 101

      !dynamic TYPE of child is NextGen     
      child1 => cbl 
      IF ( .NOT. ASSOCIATED(child1)) STOP 104
      IF ( .NOT. ALLOCATED(child1%Cmp)) STOP 105

      SELECT TYPE (child1)
         CLASS IS (NtG(knd1,*))
            !call possible only within the SELECT TYPE
            CALL foo(child1) 

         CLASS DEFAULT
            STOP 10
        END SELECT

      DEALLOCATE(cbl%Cmp)

      END PROGRAM Select_Type03c
