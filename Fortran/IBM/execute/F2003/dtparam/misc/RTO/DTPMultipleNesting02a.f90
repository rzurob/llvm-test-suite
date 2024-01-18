!*  ===================================================================
!*
!*  DATE                       : February 05, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Run Time Offset (RTO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : Default initialization
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!* Testing multilevel type nesting and accessing the components of the inner types
!*  - 3 levels of nesting
!*  - 2 levels of extension
!*
!* Defect 361702
!*
!234567890123456789012345678901234567890123456789012345678901234567890
MODULE Mod
      IMPLICIT NONE

      TYPE Base  (k1,l1)
        INTEGER, KIND :: k1 = KIND(0)
        INTEGER, LEN  :: l1 = 10

        CHARACTER(l1) :: Carr(l1) = 'AAAAA'
        REAL(k1)      :: Rarr(l1) = k1
      END TYPE Base

      TYPE, EXTENDS(Base) :: Child (k2,l2)
        INTEGER, KIND :: k2 = KIND(0)
        INTEGER, LEN  :: l2 = 10

        INTEGER(k1)                 :: Iarr(l1) = -3
        CLASS(Base(k2,l2)), POINTER :: ptr
      END TYPE Child

      TYPE Branch  (q3,n1,n2,n3)
        INTEGER, KIND :: q3 = KIND(0)
        INTEGER, LEN  :: n1 = 2, n2 = 2, n3 = 2

        TYPE(Child(q3,n1,q3,n2)) :: cmp1(n3)
        TYPE(Child(q3,n2,q3,n2)) :: cmp2(n3)
      END TYPE Branch
END MODULE Mod
PROGRAM DTPMultipleNesting02a
      USE Mod
      IMPLICIT NONE

      TYPE(Branch(4,:,:,:)), ALLOCATABLE  :: b0
      INTEGER :: i

      ALLOCATE( Branch(4,2,5,10):: b0 )

      DO i = 1, 10
        ALLOCATE( Base(4,5) :: b0%cmp1(i)%ptr )
        IF ( LEN(b0%cmp1(i)%ptr%Carr)       .NE.        5 )  STOP 11
        IF ( ANY((b0%cmp1(i)%ptr%Carr)      .NE. 'AAAAA') )  STOP 12
        IF ( SIZE(b0%cmp1(i)%ptr%Rarr)      .NE.        5 )  STOP 13
        IF ( LBOUND(b0%cmp1(i)%ptr%Rarr, 1) .NE.        1 )  STOP 14
        IF ( UBOUND(b0%cmp1(i)%ptr%Rarr, 1) .NE.        5 )  STOP 15
        IF ( ANY((b0%cmp1(i)%ptr%Rarr)      .NE.       4) )  STOP 16

        ALLOCATE( Base(4,5) :: b0%cmp2(i)%ptr )
        IF ( LEN(b0%cmp2(i)%ptr%Carr)       .NE.        5 )  STOP 17
        IF ( ANY((b0%cmp2(i)%ptr%Carr)      .NE. 'AAAAA') )  STOP 18
        IF ( SIZE(b0%cmp2(i)%ptr%Rarr)      .NE.        5 )  STOP 19
        IF ( LBOUND(b0%cmp2(i)%ptr%Rarr, 1) .NE.        1 )  STOP 20
        IF ( UBOUND(b0%cmp2(i)%ptr%Rarr, 1) .NE.        5 )  STOP 21
        IF ( ANY((b0%cmp2(i)%ptr%Rarr)      .NE.       4) )  STOP 22
      END DO

END PROGRAM DTPMultipleNesting02a
