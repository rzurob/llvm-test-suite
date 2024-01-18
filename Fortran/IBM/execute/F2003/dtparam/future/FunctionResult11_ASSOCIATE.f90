!*  ===================================================================
!*
!*  DATE                       : March 25, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Function result
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*  Defect 363239
!*
!234567890123456789012345678901234567890123456789012345678901234567890
MODULE Mod
      TYPE :: Base(k1,l1)
        INTEGER, KIND :: k1
        INTEGER, LEN :: l1

        CHARACTER(LEN=l1) :: tag="Niels"
      END TYPE

      INTERFACE foo
        FUNCTION foo(Arg)
          IMPORT Base
          CLASS(Base(4,*)) :: Arg
          TYPE(Base(4,Arg%l1)) :: Obj, foo
        END FUNCTION
      END INTERFACE

END MODULE
PROGRAM FunctionResult10
      USE Mod
      IMPLICIT NONE
      TYPE(Base(4,5)) :: b1, b2 = Base(4,5)("Bohr")
      CLASS(Base(4,:)), POINTER :: ptr

      print *, foo(b1)

      print *, foo(b2)

      ASSOCIATE ( a => foo(Base(4,6)("Henrik")) )
         print *, a
      END ASSOCIATE

      ALLOCATE ( ptr, SOURCE = Base(4,11) ("Schrodinger") )

      print *, foo(ptr)

END PROGRAM FunctionResult10
FUNCTION foo(Arg)
  USE Mod, ONLY: Base
  CLASS(Base(4,*)) :: Arg
  TYPE(Base(4,Arg%l1)) :: Obj, foo
    Obj = Arg
    foo = Obj
END FUNCTION
