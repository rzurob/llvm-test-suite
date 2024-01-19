!*  ===================================================================
!*
!*                               DTP-SELECT TYPE Construct
!*
!*  DATE                       : August 25, 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : SELECT TYPE Construct - Derived-type parameters
!*  SECONDARY FUNCTIONS TESTED : Nested SELECT TYPE Construct
!*                               Argument association (external subroutines)
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
!* See defect 355394
!*
!234567890123456789012345678901234567890123456789012345678901234567890
MODULE Mod1
      IMPLICIT NONE

      TYPE Base  (k1,l1)
        INTEGER, KIND :: k1 = KIND(0)
        INTEGER, LEN  :: l1 = 20

        INTEGER(KIND=k1) :: my_arr(l1)
      END TYPE Base

      TYPE, EXTENDS(Base) :: Child
        CLASS(*), POINTER :: Cmp
      END TYPE Child

      INTEGER, PARAMETER :: knd1 = KIND(0), len1 = 10

END MODULE Mod1

PROGRAM Select_Type02d
      USE Mod1
      IMPLICIT NONE

      INTERFACE
         SUBROUTINE Sub2(q)
           USE Mod1
           CLASS(*), INTENT(INOUT) :: q
         END SUBROUTINE Sub2
      END INTERFACE

      CLASS(*), POINTER :: pntr

      ALLOCATE( Child(knd1,len1):: pntr )
      IF (.NOT. ASSOCIATED(pntr)) ERROR STOP 10

      CALL sub2(pntr)

      DEALLOCATE ( pntr )

END PROGRAM Select_Type02d

SUBROUTINE sub2(Obj)
      USE Mod1
      IMPLICIT NONE

      CLASS(*), INTENT(INOUT) :: Obj
      TYPE(Base(knd1,len1)), TARGET :: tgt
      INTEGER :: K, sum1

      tgt = Base(knd1,len1)( my_arr = ((/ (K, K = 1, len1) /)) )
      sum1 = SUM(tgt%my_arr)

      SELECT TYPE ( A => Obj)
        TYPE IS (Child(knd1,*))
           A%Cmp => tgt
           IF ( .NOT. ASSOCIATED(A%Cmp) ) ERROR STOP 11
           ASSOCIATE ( p => A%Cmp)
              CALL sub1(p)
           END ASSOCIATE

        CLASS IS (Child(knd1,*))
           STOP 20

        CLASS IS (Base(knd1,*))
           STOP 21

        CLASS DEFAULT
           STOP 22
      END SELECT

      CONTAINS

      SUBROUTINE sub1(Obj)

      CLASS(*) :: Obj

      SELECT TYPE (A => Obj)
        TYPE IS (Child(knd1,*))
           STOP 30

        CLASS IS (Base(knd1,*))
           !* Begin nested SELECT TYPE
            SELECT TYPE (A)
              TYPE IS (Child(knd1,*))
                STOP 40

              TYPE IS (Base(knd1,*))
                IF ( SUM(A%my_arr)       .NE. sum1 ) ERROR STOP 12
                IF ( SIZE(A%my_arr)      .NE. len1 ) ERROR STOP 13
                IF ( UBOUND(A%my_arr, 1) .NE. len1 ) ERROR STOP 14
                IF ( LBOUND(A%my_arr, 1) .NE.    1 ) ERROR STOP 15

              CLASS DEFAULT
                STOP 41
            END SELECT
           !* End nested SELECT TYPE

        CLASS DEFAULT
           STOP 31
      END SELECT

      END SUBROUTINE sub1

END SUBROUTINE sub2
