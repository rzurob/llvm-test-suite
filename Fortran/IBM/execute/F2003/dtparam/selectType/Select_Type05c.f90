!*  ===================================================================
!*
!*                               DTP-SELECT TYPE Construct
!*
!*  DATE                       : September 09, 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : SELECT TYPE Construct - Derived-type parameters
!*  SECONDARY FUNCTIONS TESTED : Use association - Structure constructor
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
!* See defect 356078
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      MODULE Mod1
      IMPLICIT NONE
!*
      TYPE Node  (k1,l1)
        INTEGER, KIND :: k1
        INTEGER, LEN :: l1

        CHARACTER(l1) :: tag
      END TYPE Node

      TYPE, EXTENDS(Node) :: ExtNode
        CLASS(Node(k1,l1)), POINTER :: Next => NULL()
      END TYPE ExtNode

      INTEGER, PARAMETER :: knd1 = 4 , len1 = 10
!*
      CONTAINS
!*
      FUNCTION foo(Obj)
      CLASS(*), POINTER  :: foo
      CLASS(ExtNode(k1=knd1,l1=len1)) :: Obj

      ALLOCATE (foo, source= Obj)
      IF ( .NOT. ASSOCIATED(foo)) ERROR STOP 11

      END FUNCTION foo

      END MODULE Mod1
!*
      PROGRAM Select_Type05c
      USE Mod1
      IMPLICIT NONE

      TYPE(Node(knd1,len1)), TARGET :: FirstNode = (Node(knd1,len1) (tag = 'Full'))

      SELECT TYPE (A => foo( (ExtNode(knd1,len1) (Node = Node(knd1,len1)                      &
        & (tag = 'Empty' ) &
        & , Next=FirstNode) ) ) )

        CLASS IS (Node(knd1,*))
           STOP 20

        CLASS IS (ExtNode(knd1,*))
                IF (A%k1 .NE. knd1) ERROR STOP 11
                IF (A%l1 .NE. len1) ERROR STOP 12
                IF (A%tag .NE. 'Empty') ERROR STOP 13
                IF (A%tag%len .NE. len1) ERROR STOP 14
                IF (LEN(A%tag) .NE. len1) ERROR STOP 15
                IF (A%Next%tag .NE. 'Full') ERROR STOP 16

        CLASS DEFAULT
           STOP 21

      END SELECT

      END PROGRAM Select_Type05c
