!*  ===================================================================
!*
!*  DATE                       : February 05, 2008
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
PROGRAM DTPMultipleNesting01
      USE Mod
      IMPLICIT NONE

      TYPE(Branch(4,2,5,10)) :: b0
      INTEGER :: i, stat
      CHARACTER(100) :: errmsg

      IF ( b0%n1                  .NE.    2 )    ERROR STOP 10
      IF ( b0%n2                  .NE.    5 )    ERROR STOP 11
      IF ( b0%n3                  .NE.   10 )    ERROR STOP 12

      IF ( SIZE(b0%cmp1)          .NE.   10 )    ERROR STOP 13
      IF ( LBOUND(b0%cmp1, 1)     .NE.    1 )    ERROR STOP 14
      IF ( UBOUND(b0%cmp1, 1)     .NE.   10 )    ERROR STOP 15

      DO i = 1, 10
        IF ( LEN(b0%cmp1(i)%Carr)       .NE.     2 )    ERROR STOP 16
        IF ( ANY((b0%cmp1(i)%Carr)      .NE. 'AA') )    ERROR STOP 17

        IF ( SIZE(b0%cmp1(i)%Rarr)      .NE.     2 )    ERROR STOP 18
        IF ( LBOUND(b0%cmp1(i)%Rarr, 1) .NE.     1 )    ERROR STOP 19
        IF ( UBOUND(b0%cmp1(i)%Rarr, 1) .NE.     2 )    ERROR STOP 20
        IF ( ANY((b0%cmp1(i)%Rarr)      .NE.    4) )    ERROR STOP 21

        IF ( SIZE(b0%cmp1(i)%Iarr)      .NE.     2 )    ERROR STOP 22
        IF ( LBOUND(b0%cmp1(i)%Iarr, 1) .NE.     1 )    ERROR STOP 23
        IF ( UBOUND(b0%cmp1(i)%Iarr, 1) .NE.     2 )    ERROR STOP 24
        IF ( ANY((b0%cmp1(i)%Iarr)      .NE.   -3) )    ERROR STOP 25

        ALLOCATE( Base(4,5) :: b0%cmp1(i)%ptr, STAT=stat, ERRMSG=errmsg )
        IF ( stat                           .NE.        0 )  ERROR STOP 26
        IF ( b0%cmp1(i)%ptr%l1              .NE.        5 )  ERROR STOP 27

        IF ( LEN(b0%cmp1(i)%ptr%Carr)       .NE.        5 )  ERROR STOP 28
        IF ( ANY((b0%cmp1(i)%ptr%Carr)      .NE. 'AAAAA') )  ERROR STOP 29

        IF ( SIZE(b0%cmp1(i)%ptr%Rarr)      .NE.        5 )  ERROR STOP 30
        IF ( LBOUND(b0%cmp1(i)%ptr%Rarr, 1) .NE.        1 )  ERROR STOP 31
        IF ( UBOUND(b0%cmp1(i)%ptr%Rarr, 1) .NE.        5 )  ERROR STOP 32
        IF ( ANY((b0%cmp1(i)%ptr%Rarr)      .NE.       4) )  ERROR STOP 33
      END DO

      IF ( SIZE(b0%cmp2)          .NE.   10 )    ERROR STOP 34
      IF ( LBOUND(b0%cmp2, 1)     .NE.    1 )    ERROR STOP 35
      IF ( UBOUND(b0%cmp2, 1)     .NE.   10 )    ERROR STOP 36

      DO i = 1, 10
        IF ( LEN(b0%cmp2(i)%Carr)       .NE.        5 )    ERROR STOP 37
        IF ( ANY((b0%cmp2(i)%Carr)      .NE. 'AAAAA') )    ERROR STOP 38

        IF ( SIZE(b0%cmp2(i)%Rarr)      .NE.     5 )    ERROR STOP 39
        IF ( LBOUND(b0%cmp2(i)%Rarr, 1) .NE.     1 )    ERROR STOP 40
        IF ( UBOUND(b0%cmp2(i)%Rarr, 1) .NE.     5 )    ERROR STOP 41
        IF ( ANY((b0%cmp2(i)%Rarr)      .NE.    4) )    ERROR STOP 42

        IF ( SIZE(b0%cmp2(i)%Iarr)      .NE.     5 )    ERROR STOP 43
        IF ( LBOUND(b0%cmp2(i)%Iarr, 1) .NE.     1 )    ERROR STOP 44
        IF ( UBOUND(b0%cmp2(i)%Iarr, 1) .NE.     5 )    ERROR STOP 45
        IF ( ANY((b0%cmp2(i)%Iarr)      .NE.   -3) )    ERROR STOP 46

        ALLOCATE( b0%cmp2(i)%ptr, source = Base(4,5)('XLFtest', 8) , STAT=stat, ERRMSG=errmsg )
        IF ( stat                           .NE.        0 )  ERROR STOP 47
        IF ( b0%cmp2(i)%ptr%l1              .NE.        5 )  ERROR STOP 48

        IF ( LEN(b0%cmp2(i)%ptr%Carr)       .NE.        5 )  ERROR STOP 49
        IF ( ANY((b0%cmp2(i)%ptr%Carr)      .NE. 'XLFte') )  ERROR STOP 50

        IF ( SIZE(b0%cmp2(i)%ptr%Rarr)      .NE.        5 )  ERROR STOP 51
        IF ( LBOUND(b0%cmp2(i)%ptr%Rarr, 1) .NE.        1 )  ERROR STOP 52
        IF ( UBOUND(b0%cmp2(i)%ptr%Rarr, 1) .NE.        5 )  ERROR STOP 53
        IF ( ANY((b0%cmp2(i)%ptr%Rarr)      .NE.       8) )  ERROR STOP 54
      END DO

END PROGRAM DTPMultipleNesting01
