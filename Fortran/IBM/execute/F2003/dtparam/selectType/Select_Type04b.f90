!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : Select_Type04b - SELECT TYPE 
!*                               DTP-SELECT TYPE Construct
!*
!*  PROGRAMMER                 : Dorra Bouchiha 
!*  DATE                       : September 04, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : SELECT TYPE Construct - Derived-type parameters
!*  SECONDARY FUNCTIONS TESTED : Use association - Nested select type
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
!* See defect 355886 
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      MODULE Mod1
      IMPLICIT NONE
!*
      TYPE Node  (k1,l1)
        INTEGER, KIND :: k1 
        INTEGER, LEN :: l1 

        CHARACTER(l1) :: tag="Empty" 
        CLASS(Node(k1,l1)), POINTER :: Next => NULL()
      END TYPE Node 

      TYPE, EXTENDS(Node) :: ExtNode
      END TYPE ExtNode 

      INTEGER, PARAMETER :: knd1 = 2 , len1 =10 
!*
      CONTAINS 
!*
      FUNCTION foo(Obj)
      CLASS(*), POINTER  :: foo 
      CLASS(Node(k1=knd1,l1=len1)), POINTER :: Obj 

      foo => Obj
      IF ( .NOT. ASSOCIATED(foo)) STOP 11

      END FUNCTION foo

      SUBROUTINE Sub1(T)
      CLASS(*) ::  T

      Outer_SelType: SELECT TYPE (T)
        CLASS IS (ExtNode(knd1,*))
          IF ( .NOT. ASSOCIATED(T%Next)) STOP 5
          IF ( .NOT. ASSOCIATED(foo(T%Next))) STOP 5

         Inner_SelType: SELECT TYPE ( A => foo(T%Next)) ! call to foo possible only within the select type 
             TYPE IS (Node(knd1,*))
                IF (A%k1 .NE. knd1) STOP 112
                IF (A%l1 .NE. len1) STOP 113
                IF (A%tag .NE. 'Empty') STOP 114

          CLASS DEFAULT
            STOP 42

          END SELECT Inner_SelType

        CLASS IS (Node(knd1,*))
           STOP 31

        CLASS DEFAULT
           STOP 32

      END SELECT Outer_SelType

      END SUBROUTINE
!*
      END MODULE Mod1
!*
      PROGRAM Select_Type04b
      USE Mod1
      IMPLICIT NONE 

      TYPE(ExtNode(knd1,len1))  :: ActiveNode, tmp
      TYPE(Node(knd1,len1)), TARGET :: FirstNode = (Node(knd1,len1) ())

      ActiveNode%Next  => FirstNode
      IF ( .NOT. ASSOCIATED(ActiveNode%Next)) STOP 12

      CALL Sub1(ActiveNode)

      END PROGRAM Select_Type04b
