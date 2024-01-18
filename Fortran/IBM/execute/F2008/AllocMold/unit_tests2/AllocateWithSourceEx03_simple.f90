!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : AllocateWithSourceExp03 
!*
!*  PROGRAMMER                 : Dorra Bouchiha 
!*  DATE                       : January 20, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : ALLOCATE Statement with Source Expression 
!*  SECONDARY FUNCTIONS TESTED :
!*                               
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : 
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 
!*
!*  DESCRIPTION                :
!*
!* allocate-stmt is 
!*   ALLOCATE ( [ TYPE-spec :: ] allocation-list [, alloc-opt-list ] )
!*
!*  Defect 359514
!*
!234567890123456789012345678901234567890123456789012345678901234567890
PROGRAM AllocateWithSourceExp03
      IMPLICIT NONE 

      TYPE Base  (k1,l1)
        INTEGER, KIND :: k1 = KIND(0)
        INTEGER, LEN  :: l1 = 10

        CHARACTER(l1)  :: Carr(l1) = 'ABCD '
        INTEGER(k1) :: Iarr(l1) = 1
      END TYPE Base

      TYPE, EXTENDS(Base) :: Child (k2,l2)
        INTEGER, KIND :: k2 = KIND(0)
        INTEGER, LEN  :: l2 = 10

        TYPE(Base(k2,l2)) :: Cmp
      END TYPE Child

      TYPE(Base(4,2)) :: b1 = Base(4,2) ('Werner', 1)
      TYPE(Child(4,2,4,2)), POINTER :: c2

      ALLOCATE(c2, SOURCE = func(b1))

      CONTAINS
!*
      FUNCTION func(Arg)
       TYPE(Base(4,*)) :: Arg                           
       TYPE(Child(4,:,4,:)), POINTER :: z,w
       TYPE(Child(4,:,4,:)), POINTER :: func
       TYPE(Child(4,:,4,:)), POINTER :: x 
       integer :: l1=10

       ALLOCATE(w, z, SOURCE = Child(4,Arg%l1,4,Arg%l1) (Carr = 'TEST', &
              & Cmp = Arg, Iarr = 99) )
       print *, z%k2
       print *, z%l2
       print *, z%k1
       print *, z%l1
       func => w 
      END FUNCTION func

END PROGRAM AllocateWithSourceExp03
