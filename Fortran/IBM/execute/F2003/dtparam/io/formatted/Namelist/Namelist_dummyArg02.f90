!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONMLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : Namelist_dummyArg02
!*                               DTP - Namelist
!*
!*  PROGRAMMER                 : Dorra Bouchiha 
!*  DATE                       : November 12, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Namelist with Intrinsic IO
!*  SECONDARY FUNCTIONS TESTED : actual arguments polymorphic 
!*                               Assumed length dummy argument       
!*                     
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : NAMELIST 
!*
!*  DESCRIPTION                :
!*  namelist-stmt is 
!*
!*     NAMELIST  / namelist-group-name / namelist-group-object-list &
!*     [ [ , ] / namelist-group-name / namelist-group-object-list ] . . .
!*
!*         
!*     defect 353309
!234567890123456789012345678901234567890123456789012345678901234567890
      MODULE Mod1
      IMPLICIT NONE 

      TYPE Base (k1,l1)
        INTEGER, KIND :: k1 
        INTEGER, LEN  :: l1 

        INTEGER :: value = 10
      END TYPE Base 

      TYPE, EXTENDS(Base) :: Child (k2,l2)
        INTEGER, KIND :: k2 
        INTEGER, LEN  :: l2 

        REAL(k1) :: Rarr(l1) = 1.0D0
      END TYPE Child 

      TYPE, EXTENDS(Child) :: NextGen (k3,l3)
        INTEGER, KIND :: k3
        INTEGER, LEN  :: l3
 
        CHARACTER(l1+l2+l3) :: note='ABC'
        INTEGER(k2)   :: Iarr(l1+l2) = 2
      END TYPE NextGen

      END MODULE Mod1
!*
      PROGRAM Namelist_dummyArg02
      USE MOD1
 
      CLASS(Base(4,:)), POINTER            :: b1
      CLASS(Child(4,:,4,:)), POINTER       :: c1
      CLASS(NextGen(4,:,4,:,1,:)), POINTER :: n1
 
      ALLOCATE(NextGen(4,2,4,5,1,10) :: b1, c1, n1)
       
      call sub2(b1,c1,n1)

      CONTAINS 
!*
      SUBROUTINE sub1(argb1, argc1, argn1)
       TYPE(Base(4,*))                     :: argb1
       TYPE(Child(4,*,4,*))                :: argc1
       TYPE(NextGen(4,*,4,*,1,*))          :: argn1

       NAMELIST /NMLb/argb1, /NMLc/argc1, /NMLn/argn1
       NAMELIST /NMLt/argb1,argc1,argn1
 
       WRITE(*, NML=NMLb)
       WRITE(*, NML=NMLc)
       WRITE(*, NML=NMLn)
       WRITE(*, NML=NMLt)
 
      END SUBROUTINE sub1
 
      SUBROUTINE sub2(x,y,z) 
       CLASS(Base(4,*))            :: x 
       CLASS(Child(4,*,4,*))       :: y 
       CLASS(NextGen(4,*,4,*,1,*)) :: z 

      call sub1(x,y,z)   

      END SUBROUTINE sub2
!*
      END PROGRAM Namelist_dummyArg02
