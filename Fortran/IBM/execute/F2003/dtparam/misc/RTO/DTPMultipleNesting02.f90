!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : DTPMultipleNesting02
!*
!*  PROGRAMMER                 : Dorra Bouchiha
!*  DATE                       : February 05, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Run Time Offset (RTO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*
!*  DRIVER STANZA              : xlf2003
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
PROGRAM DTPMultipleNesting02
      USE Mod
      IMPLICIT NONE 

      TYPE(Branch(4,:,:,:)), ALLOCATABLE  :: b0
      INTEGER :: i

      ALLOCATE( Branch(4,2,5,10):: b0 )

      IF ( b0%n1                  .NE.    2 )    STOP 10
      IF ( b0%n2                  .NE.    5 )    STOP 11
      IF ( b0%n3                  .NE.   10 )    STOP 12

      IF ( SIZE(b0%cmp1)          .NE.   10 )    STOP 13
      IF ( LBOUND(b0%cmp1, 1)     .NE.    1 )    STOP 14
      IF ( UBOUND(b0%cmp1, 1)     .NE.   10 )    STOP 15

      DO i = 1, 10
        IF ( LEN(b0%cmp1(i)%Carr)       .NE.     2 )    STOP 16
        IF ( ANY((b0%cmp1(i)%Carr)      .NE. 'AA') )    STOP 17

        IF ( SIZE(b0%cmp1(i)%Rarr)      .NE.     2 )    STOP 18
        IF ( LBOUND(b0%cmp1(i)%Rarr, 1) .NE.     1 )    STOP 19
        IF ( UBOUND(b0%cmp1(i)%Rarr, 1) .NE.     2 )    STOP 20
        IF ( ANY((b0%cmp1(i)%Rarr)      .NE.    4) )    STOP 21

        IF ( SIZE(b0%cmp1(i)%Iarr)      .NE.     2 )    STOP 22
        IF ( LBOUND(b0%cmp1(i)%Iarr, 1) .NE.     1 )    STOP 23
        IF ( UBOUND(b0%cmp1(i)%Iarr, 1) .NE.     2 )    STOP 24
        IF ( ANY((b0%cmp1(i)%Iarr)      .NE.   -3) )    STOP 25

        ALLOCATE( Base(4,5) :: b0%cmp1(i)%ptr )
        IF ( b0%cmp1(i)%ptr%l1              .NE.        5 )  STOP 26 

        IF ( LEN(b0%cmp1(i)%ptr%Carr)       .NE.        5 )  STOP 27
        IF ( ANY((b0%cmp1(i)%ptr%Carr)      .NE. 'AAAAA') )  STOP 28

        IF ( SIZE(b0%cmp1(i)%ptr%Rarr)      .NE.        5 )  STOP 29
        IF ( LBOUND(b0%cmp1(i)%ptr%Rarr, 1) .NE.        1 )  STOP 30
        IF ( UBOUND(b0%cmp1(i)%ptr%Rarr, 1) .NE.        5 )  STOP 31
        IF ( ANY((b0%cmp1(i)%ptr%Rarr)      .NE.       4) )  STOP 32
      END DO 

      IF ( SIZE(b0%cmp2)          .NE.   10 )    STOP 33
      IF ( LBOUND(b0%cmp2, 1)     .NE.    1 )    STOP 34
      IF ( UBOUND(b0%cmp2, 1)     .NE.   10 )    STOP 35

      DO i = 1, 10
        IF ( LEN(b0%cmp2(i)%Carr)       .NE.        5 )    STOP 36
        IF ( ANY((b0%cmp2(i)%Carr)      .NE. 'AAAAA') )    STOP 37

        IF ( SIZE(b0%cmp2(i)%Rarr)      .NE.     5 )    STOP 38
        IF ( LBOUND(b0%cmp2(i)%Rarr, 1) .NE.     1 )    STOP 39
        IF ( UBOUND(b0%cmp2(i)%Rarr, 1) .NE.     5 )    STOP 40
        IF ( ANY((b0%cmp2(i)%Rarr)      .NE.    4) )    STOP 41

        IF ( SIZE(b0%cmp2(i)%Iarr)      .NE.     5 )    STOP 42
        IF ( LBOUND(b0%cmp2(i)%Iarr, 1) .NE.     1 )    STOP 43
        IF ( UBOUND(b0%cmp2(i)%Iarr, 1) .NE.     5 )    STOP 44
        IF ( ANY((b0%cmp2(i)%Iarr)      .NE.   -3) )    STOP 45

        ALLOCATE( Base(4,5) :: b0%cmp2(i)%ptr )
        IF ( b0%cmp2(i)%ptr%l1              .NE.        5 )  STOP 46 

        IF ( LEN(b0%cmp2(i)%ptr%Carr)       .NE.        5 )  STOP 47
        IF ( ANY((b0%cmp2(i)%ptr%Carr)      .NE. 'AAAAA') )  STOP 48

        IF ( SIZE(b0%cmp2(i)%ptr%Rarr)      .NE.        5 )  STOP 49
        IF ( LBOUND(b0%cmp2(i)%ptr%Rarr, 1) .NE.        1 )  STOP 50
        IF ( UBOUND(b0%cmp2(i)%ptr%Rarr, 1) .NE.        5 )  STOP 51
        IF ( ANY((b0%cmp2(i)%ptr%Rarr)      .NE.       4) )  STOP 52
      END DO 

END PROGRAM DTPMultipleNesting02
