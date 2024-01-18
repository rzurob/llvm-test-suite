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
!234567890123456789012345678901234567890123456789012345678901234567890
      MODULE Mod1
      IMPLICIT NONE
!*
      TYPE Node  (k1,l1)
        INTEGER, KIND :: k1
        INTEGER, LEN :: l1

        REAL(KIND=k1), DIMENSION(l1) :: array
      END TYPE Node

      TYPE, EXTENDS(Node) :: Node1
      END TYPE Node1

      TYPE, EXTENDS(Node1) :: ExtNode
        CLASS(Node(k1,l1)), POINTER :: Next => NULL()
      END TYPE ExtNode

      INTEGER, PARAMETER :: knd1 = 8 , len1 = 10
!*
      CONTAINS
!*
      FUNCTION foo(Obj)
      CLASS(*), POINTER  :: foo
      CLASS(ExtNode(k1=knd1,l1=len1)) :: Obj

      ALLOCATE (foo, source= Obj)
      IF ( .NOT. ASSOCIATED(foo)) STOP 11

      END FUNCTION foo

      END MODULE Mod1
!*
      PROGRAM Select_Type05a
      USE Mod1
      IMPLICIT NONE

      TYPE(Node(knd1,len1)), TARGET :: FirstNode = (Node(knd1,len1) (array = -1))

      SELECT TYPE (A => foo( (ExtNode(knd1,len1) (Node = Node(knd1,len1)                      &
        & (array = (/1.1_8, 2.2_8, 3.3_8, 4.4_8, 5.5_8, 6.6_8, 7.7_8, 8.8_8, 9.9_8, 10._8/) ) &
        & , Next=FirstNode) ) ) )

        CLASS IS (Node(knd1,*))
           STOP 20

        CLASS IS (Node1(knd1,*))
                IF (A%k1 .NE. knd1) STOP 11
                IF (A%l1 .NE. len1) STOP 12
                IF (A%array(1)%kind .NE. knd1) STOP 13
                IF (SIZE(A%array) .NE. len1) STOP 14

        CLASS DEFAULT
           STOP 21

      END SELECT

      END PROGRAM Select_Type05a
