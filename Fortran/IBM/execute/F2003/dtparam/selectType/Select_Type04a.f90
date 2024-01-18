!*  ===================================================================
!*
!*                               DTP-SELECT TYPE Construct
!*
!*  DATE                       : August 26, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : SELECT TYPE Construct - Derived-type parameters
!*  SECONDARY FUNCTIONS TESTED : Use association
!*                               Selector being a function call
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
      IMPLICIT CLASS(Base(2,5))(D)
!*
      TYPE Base  (k1,len1)
        INTEGER, KIND :: k1
        INTEGER, LEN :: len1
      END TYPE Base

      TYPE, EXTENDS(Base) :: Child
        CLASS(Base(k1,len1)), POINTER :: Cmp
      END TYPE Child

      TYPE, EXTENDS(Child) :: NextGen
      END TYPE NextGen

      INTEGER, PARAMETER :: k1 = 2 , len1 = 5
!*
      CONTAINS
!*
      FUNCTION foo(Obj)
      CLASS(*)  :: Obj
      CLASS(*), ALLOCATABLE  :: foo

      ALLOCATE(foo, source=Obj)
      IF ( .NOT. ALLOCATED(foo)) STOP 10

      END FUNCTION foo

      FUNCTION Dfoo(Obj)
      POINTER  :: Dfoo
      CLASS(Base(2,5)), POINTER :: Obj

      Dfoo => Obj
      IF ( .NOT. ASSOCIATED(Dfoo)) STOP 11

      END FUNCTION Dfoo
!*
      END MODULE Mod1
!*
      PROGRAM Select_Type04a
      USE Mod1
      IMPLICIT NONE

      INTEGER :: I, J
      TYPE(Child(k1,len1)) :: child1
      TYPE(Base(k1,len1)), TARGET :: tgt = (Base(k1,len1) ())

      child1%Cmp  => tgt
      IF ( .NOT. ASSOCIATED(child1%Cmp)) STOP 12


      Test_foo : SELECT TYPE ( A => foo(child1))
        TYPE IS (Child(k1,*))
           IF (A%k1 .NE. k1) STOP 100
           IF (A%len1 .NE. len1) STOP 101

        CLASS IS (Child(k1,*))
           STOP 21

        CLASS IS (NextGen(k1,*))
           STOP 22

        CLASS DEFAULT
           STOP 23

      END SELECT Test_foo

      Test_Dfoo : SELECT TYPE ( A => Dfoo(child1%Cmp))
        CLASS IS (Base(k1,*))
           IF (A%k1 .NE. k1) STOP 102
           IF (A%len1 .NE. len1) STOP 103

        CLASS IS (Child(k1,*))
           STOP 31

        CLASS IS (NextGen(k1,*))
           STOP 32

        CLASS DEFAULT
           STOP 33

      END SELECT Test_Dfoo

      END PROGRAM Select_Type04a
