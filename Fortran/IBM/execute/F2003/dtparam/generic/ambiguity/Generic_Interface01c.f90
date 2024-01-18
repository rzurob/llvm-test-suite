!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : Generic_Interface01c
!*                               DTP - Generic Interface
!*
!*  PROGRAMMER                 : Dorra Bouchiha 
!*  DATE                       : October 02, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Generic Resolution - Derived-type parameters
!*  SECONDARY FUNCTIONS TESTED : Resolution for polymorphic objects
!*                               based on the number of arguments
!*                     
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : GENERIC
!*
!*  DESCRIPTION                :
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      MODULE Mod1
      IMPLICIT NONE 

      TYPE Base  (k1,l1)
        INTEGER, KIND :: k1 
        INTEGER, LEN :: l1 
      END TYPE Base 

      TYPE, EXTENDS(Base) :: Child (k2)
        INTEGER, KIND :: k2 
      END TYPE Child 

      TYPE, EXTENDS(Child) :: NextGen(k3)
        INTEGER, KIND :: k3 
      END TYPE NextGen

      INTERFACE FUNC
         CLASS(Base(4,:)) FUNCTION foo1(Obj)
            IMPORT BASE, CHILD, NEXTGEN
            CLASS(Base(4,*)) :: Obj
            POINTER  :: foo1
         END FUNCTION foo1

         CLASS(NextGen(4,:,4,4)) FUNCTION foo2(Obj,Arg)
            IMPORT BASE, CHILD, NEXTGEN
            CLASS(NextGen(4,*,4,4)) :: Obj
            CLASS(Base(4,*)) :: Arg
            POINTER  :: foo2
         END FUNCTION foo2
      END INTERFACE

      END MODULE Mod1
!*
      CLASS(Base(4,:)) FUNCTION foo1(Obj)
         USE MOD1, Only: Base, Child, NextGen
         CLASS(Base(4,*)) :: Obj
         POINTER  :: foo1

         ALLOCATE (foo1, source = Obj)
         IF ( .NOT. ASSOCIATED(foo1)) STOP 30
      END FUNCTION foo1

      CLASS(NextGen(4,:,4,4)) FUNCTION foo2(Obj,Arg)
         USE MOD1, Only: Base, Child, NextGen
         CLASS(NextGen(4,*,4,4)) :: Obj
         CLASS(Base(4,*)) :: Arg 
         POINTER  :: foo2

         ALLOCATE (foo2, source = Obj)
         IF ( .NOT. ASSOCIATED(foo2)) STOP 31
      END FUNCTION foo2
!*
      PROGRAM Generic_Interface01c
      USE MOD1
      IMPLICIT NONE 

      CLASS(Base(4,:)), POINTER :: poly1 
      TYPE(Child(4,5,4)), TARGET :: tgt1
      TYPE(Base(4,5))  :: base1 
      TYPE(NextGen(4,10,4,4)) :: dtv    

      IF (.NOT. ASSOCIATED(FUNC(base1)) ) STOP 10

      ALLOCATE(Base(4,10):: poly1)          ! dynamic type BASE call foo1
      IF ( .NOT. ASSOCIATED(FUNC(poly1)) ) STOP 11

      poly1 => tgt1                         ! dynamic type Child call foo1
      IF ( .NOT. ASSOCIATED(FUNC(poly1)) ) STOP 12

      ALLOCATE(NextGen(4,10,4,4):: poly1)   ! dynamic type NextGen call foo1
      IF ( .NOT. ASSOCIATED(FUNC(poly1)) ) STOP 13

      SELECT TYPE ( poly1) ! call possible only within select type
          CLASS IS (NextGen(4,*,4,4))
           IF ( .NOT. ASSOCIATED(FUNC(poly1,base1)) ) STOP 14
           IF ( .NOT. ASSOCIATED(FUNC(poly1,tgt1)) ) STOP 15
           IF ( .NOT. ASSOCIATED(FUNC(poly1,poly1)) ) STOP 16

          CLASS DEFAULT
           STOP 32  
      END SELECT

      ALLOCATE(NextGen(4,10,8,8):: poly1)   ! dynamic type NextGen with k2=k3=8, call foo1
      IF ( .NOT. ASSOCIATED(FUNC(poly1))) STOP 17

      IF ( .NOT. ASSOCIATED(FUNC(dtv,base1) )) STOP 18
      IF ( .NOT. ASSOCIATED(FUNC(dtv,tgt1) )) STOP 19
      IF ( .NOT. ASSOCIATED(FUNC(dtv,poly1) )) STOP 20
      IF ( .NOT. ASSOCIATED(FUNC(dtv,dtv) )) STOP 21

      END PROGRAM Generic_Interface01c
