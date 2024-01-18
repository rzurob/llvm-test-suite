!*  ===================================================================
!*
!*                               DTP-SELECT TYPE Construct
!*
!*  DATE                       : August 12, 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : SELECT TYPE Construct - Derived-type parameters
!*  SECONDARY FUNCTIONS TESTED : Polymorphic argument association (external subroutines)
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

      INTEGER, PARAMETER :: knd1 = KIND(0), len1 = 10

      TYPE Base  (k1,l1)
        INTEGER, KIND :: k1 = KIND(0)
        INTEGER, LEN  :: l1 = 20
      END TYPE Base

      TYPE, EXTENDS(Base) :: Child
        CLASS(Base(k1,l1)), POINTER :: Cmp
      END TYPE Child

      TYPE, EXTENDS(Child) :: NextGen
      END TYPE NextGen

      END MODULE Mod1

      PROGRAM Select_Type02b
      USE Mod1
      IMPLICIT NONE

      CLASS(Base(knd1,len1)), POINTER :: Ptr

      INTERFACE
         SUBROUTINE Sub1(p)
           USE Mod1
           CLASS(NextGen(knd1,len1)), INTENT(IN) :: p
         END SUBROUTINE Sub1

         SUBROUTINE Sub2(q)
           USE Mod1
           CLASS(Base(knd1,len1)), INTENT(IN) :: q
         END SUBROUTINE Sub2
      END INTERFACE

      ALLOCATE(NextGen(knd1,len1):: Ptr)
      IF ( .NOT. ASSOCIATED(Ptr)) ERROR STOP 10

      CALL sub2(Ptr)

      SELECT TYPE ( A => Ptr)
        CLASS IS (NextGen(knd1,*))
           ! call possible within SELECT TYPE only because the dynamic type of Ptr is NextGen
           CALL sub1(A)

           ALLOCATE(Child(knd1,len1):: A%Cmp)
           IF ( .NOT. ASSOCIATED(A%Cmp)) ERROR STOP 11

           ! call possible because dynamic type of A%Cmp extends type of Obj in sub2
           CALL sub2(A%Cmp)

        CLASS DEFAULT
           STOP 12
      END SELECT

END PROGRAM Select_Type02b

SUBROUTINE sub1(Obj)
        USE Mod1
        CLASS(NextGen(knd1,len1)), INTENT(IN) :: Obj

        SELECT TYPE (A => Obj)
          CLASS IS (NextGen(knd1,*))
             SELECT TYPE (A)
                CLASS DEFAULT
                   STOP 20

                TYPE is (NextGen(knd1,*))
                   IF ( A%k1 .NE. knd1) ERROR STOP 21
                   IF ( A%l1 .NE. len1) ERROR STOP 22

             END SELECT

          CLASS DEFAULT
             STOP 23
        END SELECT

END SUBROUTINE sub1

SUBROUTINE sub2(Obj)
        USE Mod1
        CLASS(Base(knd1,len1)), INTENT(IN) :: Obj

        SELECT TYPE (A => Obj)
           CLASS IS (Child(knd1,*))
              IF ( A%k1 .NE. knd1) ERROR STOP 30
              IF ( A%l1 .NE. len1) ERROR STOP 31
              IF ( ASSOCIATED(A%Cmp)) ERROR STOP 32

          CLASS IS (NextGen(knd1,*))
              IF ( A%k1 .NE. knd1) ERROR STOP 33
              IF ( A%l1 .NE. len1) ERROR STOP 34
              IF ( ASSOCIATED(A%Cmp)) ERROR STOP 35

          CLASS DEFAULT
             STOP 36
        END SELECT

END SUBROUTINE sub2
