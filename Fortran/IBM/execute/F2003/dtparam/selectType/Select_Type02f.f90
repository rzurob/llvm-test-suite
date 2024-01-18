!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : Select_Type02f - SELECT TYPE 
!*                               DTP-SELECT TYPE Construct
!*
!*  PROGRAMMER                 : Dorra Bouchiha 
!*  DATE                       : August 26, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : SELECT TYPE Construct - Derived-type parameters
!*  SECONDARY FUNCTIONS TESTED : Argument association - external function
!*                               Selector being a function call
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

      TYPE Base (k1,l1)
        INTEGER, KIND :: k1 
        INTEGER, LEN  :: l1 
      END TYPE Base 

      TYPE, EXTENDS(Base) :: Child
        CLASS(Base(k1,l1)), POINTER :: Cmp 
      END TYPE Child 

      TYPE, EXTENDS(Child) :: NextGen
      END TYPE NextGen

      INTEGER, PARAMETER :: knd1 = 2 , len1 = 5
END MODULE Mod1
!*
PROGRAM Select_Type02f
      USE Mod1
      IMPLICIT NONE 

      TYPE(Child(knd1,len1)) :: child1 
      TYPE(Base(knd1,len1)), TARGET :: tgt = Base(knd1,len1) ()

      INTERFACE
        FUNCTION foo(Obj)
          USE Mod1
          IMPLICIT NONE 
          CLASS(*)  :: Obj
          CLASS(*), ALLOCATABLE  :: foo
        END FUNCTION foo

        FUNCTION Dfoo(Obj)
          USE Mod1
          IMPLICIT NONE 
          CLASS(Base(2,5)), POINTER  :: Dfoo
          CLASS(Base(2,5)), POINTER :: Obj
        END FUNCTION Dfoo
      END INTERFACE

      child1%Cmp  => tgt
      IF ( .NOT. ASSOCIATED(child1%Cmp) ) STOP 12

      Test_foo : SELECT TYPE ( A => foo(child1) )
        TYPE IS (Child(knd1,*))
           IF (A%k1 .NE. knd1) STOP 100
           IF (A%l1 .NE. len1) STOP 101

        CLASS IS (Child(knd1,*))
           STOP 21

        CLASS IS (NextGen(knd1,*))
           STOP 22

        CLASS DEFAULT
           STOP 23

      END SELECT Test_foo

      Test_foo2 : SELECT TYPE ( A => foo(child1%Cmp) )
        TYPE IS (Base(knd1,*))
           IF (A%k1 .NE. knd1) STOP 102
           IF (A%l1 .NE. len1) STOP 103

        TYPE IS (Child(knd1,*))
           STOP 31

        TYPE IS (NextGen(knd1,*))
           STOP 32

        CLASS DEFAULT
           STOP 33

      END SELECT Test_foo2

      Test_Dfoo : SELECT TYPE ( A => Dfoo(child1%Cmp))
        CLASS IS (Base(knd1,*))
           IF (A%k1 .NE. knd1) STOP 104
           IF (A%l1 .NE. len1) STOP 105

        CLASS IS (Child(knd1,*))
           STOP 41

        CLASS IS (NextGen(knd1,*))
           STOP 42

        CLASS DEFAULT
           STOP 43

      END SELECT Test_Dfoo
END PROGRAM Select_Type02f
!*
FUNCTION foo(Obj)
        USE Mod1
        IMPLICIT NONE 
        CLASS(*)  :: Obj
        CLASS(*), ALLOCATABLE  :: foo

        ALLOCATE(foo, source=Obj)
        IF ( .NOT. ALLOCATED(foo)) STOP 10
END FUNCTION foo

FUNCTION Dfoo(Obj)
        USE Mod1
        IMPLICIT NONE 
        CLASS(Base(2,5)), POINTER  :: Dfoo
        CLASS(Base(2,5)), POINTER :: Obj

        Dfoo => Obj
        IF ( .NOT. ASSOCIATED(Dfoo)) STOP 11
END FUNCTION Dfoo
!*

