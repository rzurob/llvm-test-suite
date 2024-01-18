!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : Generic_BOperator03
!*                               DTP - Generic Operator (binary)
!*
!*  PROGRAMMER                 : Dorra Bouchiha 
!*  DATE                       : October 03, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Generic Resolution - Derived-type parameters
!*  SECONDARY FUNCTIONS TESTED : Resolution based on rank           
!*                     
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : GENERIC
!*
!*  DESCRIPTION                :
!*
!*  R448 type-bound-procedure-part is contains-stmt
!*                                     [ binding-private-stmt ]
!*                                     proc-binding-stmt
!*                                     [ proc-binding-stmt ] ...
!*
!*  R450 proc-binding-stmt is specific-binding
!*                         or generic-binding
!*                         or final-binding
!*
!*  R451 specific-binding is PROCEDURE [ (interface-name) ] &
!*                                    & [ [, binding-attr -list ] :: ] &
!*                                    & binding-name [ => procedure-name ]
!*
!*  R452 generic-binding is GENERIC [, access-spec ] :: generic-spec => binding-name-list
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      MODULE Mod1
      IMPLICIT NONE 

      TYPE Base (k,l)
        INTEGER, KIND :: k 
        INTEGER, LEN :: l 

        INTEGER :: value

        CONTAINS 
         PROCEDURE, PASS :: m0 => mut0
         PROCEDURE, PASS :: m1 => mut1
         PROCEDURE, PASS :: m2 => mut2
         GENERIC :: operator(*) => m0, m1, m2      
      END TYPE Base 

      TYPE, EXTENDS(Base) :: Child (k1,l1)
        INTEGER, KIND :: k1 
        INTEGER, LEN :: l1 
      END TYPE Child 

      TYPE, EXTENDS(Child) :: NextGen (k13,l13)
        INTEGER, KIND :: k13
        INTEGER, LEN :: l13
      END TYPE NextGen

      CONTAINS 
!*
      TYPE(Base(4,:)) FUNCTION mut0(arg1,arg2) 
      CLASS(Base(4,*)), INTENT(IN) :: arg1 
      CLASS(Base(4,*)), INTENT(IN) :: arg2      ! rank 0
      POINTER :: mut0

      ALLOCATE(BASE(arg2%k,arg2%l) :: mut0)
      mut0%value = 4

      END FUNCTION mut0

      TYPE(Base(4,:)) FUNCTION mut1(arg1,arg2) 
      CLASS(Base(4,*)), INTENT(IN) :: arg1
      CLASS(Base(4,*)), INTENT(IN) :: arg2(:)   ! rank 1
      POINTER :: mut1

      ALLOCATE(BASE(arg2%k,arg2%l) :: mut1)
      mut1%value = 14

      END FUNCTION mut1

      TYPE(Base(4,:)) FUNCTION mut2(arg1,arg2) 
      CLASS(Base(4,*)), INTENT(IN) :: arg1
      CLASS(Base(4,*)), INTENT(IN) :: arg2(:,:)  ! rank 2
      POINTER :: mut2

      ALLOCATE(BASE(arg2%k,arg2%l) :: mut2)
      mut2%value = 24

      END FUNCTION mut2

      END MODULE Mod1
!*
      PROGRAM Generic_BOperator03
      USE MOD1
      IMPLICIT CLASS(Base(4,:))(B)
      IMPLICIT CLASS(Child(4,:,4,:))(C)
      IMPLICIT CLASS(NextGen(4,:,4,:,8,:))(N)

      POINTER :: b_var  
      POINTER :: C0, N0
      POINTER :: B1(:), C1(:)
      POINTER :: B2(:,:), N2(:,:)

      ALLOCATE ( Base(4,100) :: B1(100) )
      ALLOCATE ( Child(4,10,4,10) :: C0, C1(10), B2(10,10) )
      ALLOCATE ( NextGen(4,1,4,1,8,1) :: N0, N2(1,1) )

      B_var => C0 * C0
      IF ( B_var%value .NE. 4 ) STOP 1
      B_var => C0 * N0
      IF ( B_var%value .NE. 4 ) STOP 2
      B_var => C0 * B1
      IF ( B_var%value .NE. 14 ) STOP 3
      B_var => C0 * C1
      IF ( B_var%value .NE. 14 ) STOP 4
      B_var => C0 * B2
      IF ( B_var%value .NE. 24 ) STOP 5
      B_var => C0 * N2
      IF ( B_var%value .NE. 24 ) STOP 6

      B_var => N0 * C0
      IF ( B_var%value .NE. 4 ) STOP 7
      B_var => N0 * N0
      IF ( B_var%value .NE. 4 ) STOP 8
      B_var => N0 * B1
      IF ( B_var%value .NE. 14 ) STOP 9
      B_var => N0 * C1
      IF ( B_var%value .NE. 14 ) STOP 10
      B_var => N0 * B2
      IF ( B_var%value .NE. 24 ) STOP 11
      B_var => N0 * N2
      IF ( B_var%value .NE. 24 ) STOP 12

      END PROGRAM Generic_BOperator03
