!*  ===================================================================
!*
!*                               DTP-SELECT TYPE Construct
!*
!*  DATE                       : August 11, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : SELECT TYPE Construct - Derived-type parameters
!*  SECONDARY FUNCTIONS TESTED : Nested SELECT TYPE Construct
!*                               Host association
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
      PROGRAM Select_Type01d
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
        CLASS(*), POINTER :: Cmp
      END TYPE Child

      INTEGER, PARAMETER :: k1 = KIND(0), len1 = 10
      CLASS(*), POINTER :: pntr

      ALLOCATE(Child(k1,len1):: pntr)
      IF (.NOT. ASSOCIATED(pntr)) STOP 10

      CALL sub2

      CONTAINS
!*
      SUBROUTINE sub2

      SELECT TYPE ( A => pntr)
        TYPE IS (Child(k1,*))
           ALLOCATE(Base(k1,len1):: A%Cmp)
           IF (.NOT. ASSOCIATED(A%Cmp)) STOP 11
           CALL sub1(A%Cmp)

        CLASS IS (Child(k1,*))
           STOP 20

        CLASS IS (Base(k1,*))
           STOP 21

        CLASS DEFAULT
           STOP 22
      END SELECT

      END SUBROUTINE sub2
!*
      SUBROUTINE sub1(Obj)

      CLASS(*) :: Obj

      SELECT TYPE (A => Obj)
        TYPE IS (Child(k1,*))
           STOP 30

        CLASS IS (Base(k1,*))
           !* Begin nested SELECT TYPE
            SELECT TYPE (A)
              TYPE IS (Child(k1,*))
                STOP 40

              TYPE IS (Base(k1,*))
                IF (SIZE(A%my_arr) .NE. len1) STOP 12

              CLASS DEFAULT
                STOP 41
            END SELECT
           !* End nested SELECT TYPE

        CLASS DEFAULT
           STOP 31
      END SELECT

      END SUBROUTINE sub1
!*
      END PROGRAM Select_Type01d
