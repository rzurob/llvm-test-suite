!*  ===================================================================
!*
!*                               DTP-SELECT TYPE Construct
!*
!*  DATE                       : September 08, 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : SELECT TYPE Construct - Derived-type parameters
!*  SECONDARY FUNCTIONS TESTED : Use association - PRIVATE component
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
!* See defect 355886
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      MODULE Mod1
      IMPLICIT NONE
!*
      TYPE Node  (k1,l1)
        INTEGER, KIND :: k1
        INTEGER, LEN :: l1

        CHARACTER(l1) :: tag="Empty" ! tags can be "Empty" or "Full"
        CLASS(Node(k1,l1)), POINTER :: Next => NULL()
      END TYPE Node

      TYPE, EXTENDS(Node) :: ExtNode
         PRIVATE
         INTEGER(k1), ALLOCATABLE :: my_arr(:)
         INTEGER(k1), PUBLIC :: sum_arr
      END TYPE ExtNode

      INTEGER, PARAMETER :: knd1 = 2 , len1 =10
!*
      CONTAINS

      SUBROUTINE fill_my_arr(T)
      CLASS(*)  ::  T
      INTEGER :: I

      SELECT TYPE (T)
        CLASS IS (ExtNode(knd1,*))
         IF ( .NOT. ALLOCATED(T%my_arr))  ALLOCATE (T%my_arr(len1))
         IF ( .NOT. ALLOCATED(T%my_arr)) STOP 20

    ! select case
         SELECT CASE (T%tag)
           CASE ('Empty')
             T%my_arr = (/ (0, I = 1, len1)/)
             T%sum_arr = SUM(T%my_arr)
           CASE ('Full')
             T%my_arr = (/ (I, I = 1, len1)/)
             T%sum_arr = SUM(T%my_arr)
          CASE DEFAULT
             STOP 21
         END SELECT

        CLASS DEFAULT
           STOP 22

      END SELECT

      END SUBROUTINE

      FUNCTION foo(Obj)
      CLASS(*), POINTER  :: foo
      CLASS(Node(k1=knd1,l1=len1)), POINTER :: Obj

      foo => Obj
      IF ( .NOT. ASSOCIATED(foo)) STOP 4

      END FUNCTION foo

      END MODULE Mod1
!*
      MODULE Mod2
      USE Mod1
      IMPLICIT NONE
!*
      CONTAINS
!*
      SUBROUTINE Sub1(T)
      CLASS(*) ::  T

      Outer_SelType: SELECT TYPE (T)
        CLASS IS (ExtNode(knd1,*))
          IF ( .NOT. ASSOCIATED(T%Next)) STOP 5
          IF ( .NOT. ASSOCIATED(foo(T%Next))) STOP 6

          Inner_SelType: SELECT TYPE ( A => foo(T%Next)) ! call to foo possible only within the select type
             TYPE IS (Node(knd1,*))
                IF (A%k1 .NE. knd1) STOP 112
                IF (A%l1 .NE. len1) STOP 113
             CLASS DEFAULT
                STOP 40

          END SELECT Inner_SelType

          T%tag = 'Full'
          CALL fill_my_arr(T)
          IF (T%sum_arr .NE. 55) STOP 114

        CLASS IS (Node(knd1,*))
           STOP 30

        CLASS DEFAULT
           STOP 31

      END SELECT Outer_SelType

      END SUBROUTINE
!*
      END MODULE Mod2
!*
      PROGRAM Select_Type04d
      USE Mod1
      USE Mod2, ONLY: Sub1
      IMPLICIT NONE

      TYPE(ExtNode(knd1,len1))  :: ActiveNode
      TYPE(Node(knd1,len1)), TARGET :: FirstNode = (Node(knd1,len1) ())

      ActiveNode%Next  => FirstNode
      IF ( .NOT. ASSOCIATED(ActiveNode%Next)) STOP 12

      CALL Sub1(ActiveNode)

      END PROGRAM Select_Type04d
