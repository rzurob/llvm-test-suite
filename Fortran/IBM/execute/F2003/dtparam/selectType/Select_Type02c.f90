!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : Select_Type02c - SELECT TYPE 
!*                               DTP-SELECT TYPE Construct
!*
!*  PROGRAMMER                 : Dorra Bouchiha 
!*  DATE                       : August  12, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : SELECT TYPE Construct - Derived-type parameters
!*  SECONDARY FUNCTIONS TESTED : Allocation of a pointer inside the SELECT TYPE Construct
!*                               Argument association (external subroutines)
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
!234567890123456789012345678901234567890123456789012345678901234567890
MODULE Mod1
      IMPLICIT NONE 
      INTEGER, PARAMETER :: knd1 = KIND(0), len1 = 10

      TYPE Base  (k1,l1)
        INTEGER, KIND :: k1 = knd1
        INTEGER, LEN  :: l1 = len1 

        INTEGER(KIND=k1) :: my_arr(l1)
      END TYPE Base 

      TYPE, EXTENDS(Base) :: Child
        CLASS(Base(k1,l1)), POINTER :: Cmp
      END TYPE Child 

END MODULE Mod1

PROGRAM Select_Type02c
      USE Mod1
      IMPLICIT NONE 

      CLASS(*), ALLOCATABLE :: child1 

      INTERFACE
         SUBROUTINE Sub2(q)
           USE Mod1
           CLASS(*), ALLOCATABLE :: q
           TYPE(Base(knd1,len1)), TARGET :: tgt
           INTEGER :: I
         END SUBROUTINE Sub2
      END INTERFACE

      ALLOCATE(Child(knd1,len1):: child1)
      IF ( .NOT. ALLOCATED(child1) ) STOP 10

      CALL sub2(child1)

END PROGRAM Select_Type02c

SUBROUTINE sub2(Obj)
      USE Mod1
      CLASS(*), ALLOCATABLE :: Obj 
      TYPE(Base(knd1,len1)), TARGET :: tgt
      INTEGER :: I

      tgt%my_arr=(/ (10, I = 1, len1) /)

      SELECT TYPE (A => Obj)
        CLASS IS (Child(knd1,*))
           A%Cmp => tgt
           IF ( .NOT. ASSOCIATED(A%Cmp) ) STOP 20
           CALL sub1(A%Cmp) 

        CLASS IS (Base(knd1,*))
           STOP 21

        CLASS DEFAULT
           STOP 22
      END SELECT

     CONTAINS

      SUBROUTINE sub1(Obj)
      CLASS(Base(knd1,len1)) :: Obj

      SELECT TYPE (A => Obj)
        TYPE IS (Child(knd1,*))
          STOP 30

        TYPE IS (Base(knd1,*))
          IF ( SIZE(A%my_arr)      .NE. len1 ) STOP 31
          IF ( UBOUND(A%my_arr, 1) .NE. len1 ) STOP 32
          IF ( LBOUND(A%my_arr, 1) .NE.    1 ) STOP 33
          IF ( ANY(A%my_arr        .NE.  10) ) STOP 34

        CLASS DEFAULT
          STOP 35
      END SELECT

      END SUBROUTINE sub1

END SUBROUTINE sub2
