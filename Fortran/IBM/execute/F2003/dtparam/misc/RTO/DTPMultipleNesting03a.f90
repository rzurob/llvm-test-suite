!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : DTPMultipleNesting03a
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
!*  KEYWORD(S)                 : No default initialization
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!* Testing multilevel type nesting and accessing the components of the inner types
!*  - 3 levels of nesting
!*  - 2 levels of extension
!*
!* same as DTPMultipleNesting03 but accessing the most inner component without
!* accessing the others
!*
!* Defect 355488 and 361702 
!*
!234567890123456789012345678901234567890123456789012345678901234567890
MODULE Mod
      IMPLICIT NONE 

      TYPE Base  (k1,l1)
        INTEGER, KIND :: k1
        INTEGER, LEN  :: l1

        CHARACTER(l1) :: Carr(l1)    
        REAL(k1)      :: Rarr(l1)
      END TYPE Base

      TYPE, EXTENDS(Base) :: Child (k2,l2)
        INTEGER, KIND :: k2
        INTEGER, LEN  :: l2

        INTEGER(k1)                 :: Iarr(l1)
        CLASS(Base(k2,l2)), POINTER :: ptr
      END TYPE Child

      TYPE Branch  (q3,n1,n2,n3)
        INTEGER, KIND :: q3
        INTEGER, LEN  :: n1, n2, n3

        TYPE(Child(q3,n1,q3,n2)) :: cmp1(n3)
        TYPE(Child(q3,n2,q3,n2)) :: cmp2(n3)
      END TYPE Branch
END MODULE Mod
PROGRAM DTPMultipleNesting03a
      USE Mod
      IMPLICIT NONE 

      CLASS(Branch(4,:,:,:)), POINTER :: b0
      INTEGER :: i

      ALLOCATE( Branch(4,2,5,10) :: b0 )
      IF ( .NOT. ASSOCIATED(b0) ) STOP 10 

      IF ( b0%n1                  .NE.    2 )    STOP 11
      IF ( b0%n2                  .NE.    5 )    STOP 12
      IF ( b0%n3                  .NE.   10 )    STOP 13

      b0%cmp1 = Child(4,2,4,5) ('BB', 4.8, 11, NULL())   
      b0%cmp2 = Child(4,5,4,5) ('AAAAA', 3.1, -7, NULL()) 

      DO i = 1, 10
        ALLOCATE( b0%cmp1(i)%ptr, SOURCE = Base(b0%cmp1%k2,b0%cmp1%l2) ('Aleckes', 6.5) ) 
        IF ( .NOT. ASSOCIATED(b0%cmp1(i)%ptr) ) STOP 20                                  

        ALLOCATE( b0%cmp2(i)%ptr, SOURCE = Base(b0%cmp1%k2,b0%cmp1%l2) ('Alceeme', 0.1) ) 
        IF ( .NOT. ASSOCIATED(b0%cmp2(i)%ptr) ) STOP 21                                  

        IF ( LEN(b0%cmp1(i)%ptr%Carr)       .NE.        5 )  STOP 30
        IF ( SIZE(b0%cmp1(i)%ptr%Carr)      .NE.        5 )  STOP 31
        IF ( ANY((b0%cmp1(i)%ptr%Carr)      .NE. 'Aleck') )  STOP 32                   

        IF ( SIZE(b0%cmp1(i)%ptr%Rarr)      .NE.        5 )  STOP 33
        IF ( LBOUND(b0%cmp1(i)%ptr%Rarr, 1) .NE.        1 )  STOP 34
        IF ( UBOUND(b0%cmp1(i)%ptr%Rarr, 1) .NE.        5 )  STOP 35
        IF ( ANY((b0%cmp1(i)%ptr%Rarr)      .NE.     6.5) )  STOP 36

        IF ( LEN(b0%cmp2(i)%ptr%Carr)       .NE.        5 )  STOP 37
        IF ( SIZE(b0%cmp2(i)%ptr%Carr)      .NE.        5 )  STOP 38
        IF ( ANY((b0%cmp2(i)%ptr%Carr)      .NE. 'Alcee') )  STOP 40

        IF ( SIZE(b0%cmp2(i)%ptr%Rarr)      .NE.        5 )  STOP 41
        IF ( LBOUND(b0%cmp2(i)%ptr%Rarr, 1) .NE.        1 )  STOP 42
        IF ( UBOUND(b0%cmp2(i)%ptr%Rarr, 1) .NE.        5 )  STOP 43
        IF ( ANY((b0%cmp2(i)%ptr%Rarr)      .NE.     0.1) )  STOP 44
      END DO 

     DEALLOCATE ( b0 )

END PROGRAM DTPMultipleNesting03a
