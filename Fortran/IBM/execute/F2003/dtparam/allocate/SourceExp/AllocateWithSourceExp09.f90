!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : AllocateWithSourceExp09 
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
PROGRAM AllocateWithSourceExp09
      IMPLICIT NONE 

      TYPE Base  (k1,l1)
        INTEGER, KIND :: k1 = KIND(0)
        INTEGER, LEN  :: l1 = 10

        CHARACTER(l1)  :: Carr(l1) = 'ABCDEFGHIJ'
        INTEGER(k1) :: Iarr(l1) = 1
      END TYPE Base

      TYPE, EXTENDS(Base) :: Child (k2,l2)
        INTEGER, KIND :: k2 = KIND(0)
        INTEGER, LEN  :: l2 = 10

        CLASS(Base(k2,:)), POINTER :: b_cmp
      END TYPE Child

      CLASS(*), POINTER :: c1, b1 

      ALLOCATE(Base(4,9) :: b1)

      SELECT TYPE(b1)
        TYPE IS(Base(4,*))
           IF(b1%k1 .NE. 4) STOP 10
           IF(b1%l1 .NE. 9) STOP 11
           IF(ANY(b1%Carr .NE. 'ABCDEFGHI')) STOP 12
           IF(ANY(b1%Iarr .NE. 1)) STOP 13
           ALLOCATE(c1, SOURCE = Child(4,7,4,9)(b_cmp = b1) )

        CLASS DEFAULT
            STOP 101

      END SELECT

      SELECT TYPE(c1)
        TYPE IS(Child(4,*,4,*))
           IF(c1%k1 .NE. 4) STOP 14
           IF(c1%l1 .NE. 7) STOP 15
           IF(c1%k2 .NE. 4) STOP 16
           IF(c1%l2 .NE. 9) STOP 17
           IF(ANY(c1%Carr .NE. 'ABCDEFG')) STOP 18
           IF(ANY(c1%Iarr .NE. 1)) STOP 19
           IF(c1%b_cmp%k1 .NE. 4) STOP 20
           IF(c1%b_cmp%l1 .NE. 9) STOP 21
           IF(ANY(c1%b_cmp%Carr .NE. 'ABCDEFGHI')) STOP 22
           IF(ANY(c1%b_cmp%Iarr .NE. 1)) STOP 23

        CLASS DEFAULT
            STOP 102

      END SELECT

      ALLOCATE(b1, SOURCE = c1)

      SELECT TYPE(b1)
         TYPE IS(Child(4,*,4,*))
           IF(b1%k1 .NE. 4) STOP 24
           IF(b1%l1 .NE. 7) STOP 25
           IF(b1%k2 .NE. 4) STOP 26
           IF(b1%l2 .NE. 9) STOP 27
           IF(ANY(b1%Carr .NE. 'ABCDEFG')) STOP 28
           IF(ANY(b1%Iarr .NE. 1)) STOP 29
           IF(b1%b_cmp%k1 .NE. 4) STOP 30
           IF(b1%b_cmp%l1 .NE. 9) STOP 31
           IF(ANY(b1%b_cmp%Carr .NE. 'ABCDEFGHI')) STOP 32
           IF(ANY(b1%b_cmp%Iarr .NE. 1)) STOP 33

        CLASS DEFAULT
            STOP 103
      END SELECT

      DEALLOCATE(b1,c1) 

END PROGRAM AllocateWithSourceExp09
