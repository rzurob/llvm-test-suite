!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : Select_Type03c - SELECT TYPE 
!*                               DTP-SELECT TYPE Construct
!*
!*  PROGRAMMER                 : Dorra Bouchiha 
!*  DATE                       : August 29, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : SELECT TYPE Construct - Derived-type parameters
!*  SECONDARY FUNCTIONS TESTED : IMPLICIT TYPE mapping 
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

        INTEGER(KIND=k1) :: my_arr(l1)
      END TYPE Base 

      TYPE, EXTENDS(Base) :: Child
        CLASS(*), ALLOCATABLE :: Cmp(:) 
      END TYPE Child 

      INTEGER, PARAMETER :: knd1 = KIND(0), len1 = 5
  
      CONTAINS
!*
      SUBROUTINE sub2(Obj)
        CLASS(Child(knd1,len1)) :: Obj

          SELECT TYPE ( A => (/Obj%Cmp/) )
            TYPE IS (Child(knd1,*))
              STOP 30

          CLASS IS (Child(knd1,*))
              STOP 31

          CLASS IS (Base(knd1,*))
              IF( SIZE(A) /= len1) STOP 111

          CLASS DEFAULT
             STOP 32
          END SELECT

      END SUBROUTINE sub2

      END MODULE Mod1

      PROGRAM Select_Type03c
      USE Mod1
      IMPLICIT CLASS(Base(knd1,len1)) (D)   ! Implicit mapping of the letter D to the type Base

      TYPE(Child(knd1,len1)), TARGET :: cbl
      POINTER :: dtv
      INTEGER :: Base  ! In this scoping unit Base is not a TYPE 

      ALLOCATE(Base(knd1,len1):: cbl%Cmp(len1))
      IF ( .NOT. ALLOCATED(cbl%Cmp)) STOP 101

      !dynamic TYPE of dtv is Child       
      dtv => cbl 
      IF ( .NOT. ASSOCIATED(dtv)) STOP 104
 
      Base=0

      SELECT TYPE (dtv)
         CLASS IS (Child(knd1,*))
            !call possible only within the SELECT TYPE
            CALL sub2(dtv) 
            Base = len1

         CLASS DEFAULT
            STOP 10
        END SELECT

      IF (Base /= len1) STOP 105
      DEALLOCATE(cbl%Cmp)

      END PROGRAM Select_Type03c
