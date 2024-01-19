!*  ===================================================================
!*
!*  DATE                       : March 25, 2008
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
!* Defect: 364256
!*
!234567890123456789012345678901234567890123456789012345678901234567890
MODULE Mod
      IMPLICIT NONE

      TYPE Base (k1,l1)
        INTEGER, KIND :: k1
        INTEGER, LEN  :: l1

        INTEGER(k1) :: A0(l1)
      END TYPE Base

      TYPE,  EXTENDS(Base) :: Child (k2,l2)
        INTEGER, KIND :: k2
        INTEGER, LEN  :: l2

        TYPE(Base(k2,2*l2+1)) :: cmp
      END TYPE

      TYPE NewType (k1,l1)
        INTEGER, KIND :: k1
        INTEGER, LEN  :: l1

        CLASS(Base(k1,l1)), POINTER :: b1
        CLASS(Child(k1,l1-1,k1,l1-1)), POINTER :: c1
      END TYPE NewType

      CONTAINS

      FUNCTION foo(Arg, l1)
        CLASS(NewType(4,:)), POINTER :: Arg
        integer, intent(in) :: l1
        TYPE(NewType(4,l1)) :: foo

        foo = NewType(4,l1)( Arg%b1, Arg%c1 )

      END FUNCTION foo
END MODULE
PROGRAM FunctionResult10
      USE Mod
      IMPLICIT NONE

      CLASS(NewType(4,:)), POINTER :: ptr
      TYPE(Base(4,5)), TARGET :: tgt1
      TYPE(Child(4,4,4,4)), TARGET :: tgt2

      ALLOCATE ( ptr, SOURCE = NewType(4,9)( null(), null() ) )

      call ASSOCIATE1 ( a = foo(ptr, ptr%l1) )

      deallocate (ptr)

      ALLOCATE ( ptr, SOURCE = NewType(4,5)( tgt1, tgt2 ) )

      call ASSOCIATE2 ( foo(ptr, ptr%l1) )

      contains

!      ASSOCIATE ( a => foo(ptr, ptr%l1) )
      subroutine ASSOCIATE1 ( a )
        type(newtype(4,*)), intent(in) :: a

        IF ( a%b1%l1 .NE. 9 ) ERROR STOP 10
        IF ( a%c1%l1 .NE. 8 ) ERROR STOP 11
        IF ( a%c1%l2 .NE. 8 ) ERROR STOP 12
        IF ( ASSOCIATED(a%b1) ) ERROR STOP 13
        IF ( ASSOCIATED(a%c1) ) ERROR STOP 14
      END subroutine


!      ASSOCIATE ( a => foo(ptr, ptr%l1) )
      subroutine ASSOCIATE2 ( a )
        type(newtype(4,*)), intent(in) :: a

        IF ( .NOT. ASSOCIATED(a%b1) ) ERROR STOP 15
        IF ( .NOT. ASSOCIATED(a%c1) ) ERROR STOP 16
        IF ( a%b1%l1 .NE. 5 ) ERROR STOP 17
        IF ( a%c1%l1 .NE. 4 ) ERROR STOP 18
        IF ( a%c1%l2 .NE. 4 ) ERROR STOP 19
        IF ( a%c1%cmp%l1 .NE. 9 ) ERROR STOP 20
        IF (SIZE(a%b1%A0) .NE.  5) ERROR STOP 21
        IF (SIZE(a%c1%A0) .NE.  4) ERROR STOP 22
        IF (SIZE(a%c1%cmp%A0) .NE.  9) ERROR STOP 23
      END subroutine

END PROGRAM FunctionResult10
