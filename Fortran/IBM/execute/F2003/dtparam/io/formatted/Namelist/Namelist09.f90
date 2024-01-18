!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONMLtY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : Namelist09
!*                               DTP - Namelist
!*
!*  PROGRAMMER                 : Dorra Bouchiha 
!*  DATE                       : November 20, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Namelist with Intrinsic IO
!*  SECONDARY FUNCTIONS TESTED : None
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
!234567890123456789012345678901234567890123456789012345678901234567890
MODULE Mod1
      IMPLICIT NONE 

      TYPE Base (k1,l1)  ! (4,5)
        INTEGER, KIND :: k1 
        INTEGER, LEN  :: l1 

        CONTAINS
        PROCEDURE :: cfoo                 
      END TYPE Base 

      TYPE, EXTENDS(Base) :: Child (k2,l2)  ! (4,5)
        INTEGER, KIND :: k2 
        INTEGER, LEN  :: l2 

        REAL(k2) :: Rarr(l1) = k2
      END TYPE Child 

      TYPE, EXTENDS(Child) :: NextGen (k3,l3) ! (1,7)
        INTEGER, KIND :: k3
        INTEGER, LEN  :: l3
 
        CHARACTER(l2) :: Carr(l2) = 'ABCD '
        INTEGER(k2)   :: Iarr(l1+l2) = k3
      END TYPE NextGen

      CONTAINS
 
      FUNCTION cfoo(Arg)
       CLASS(Base(4,*)) :: Arg
       COMPLEX cfoo 
         cfoo = (Arg%k1,Arg%l1)
      END FUNCTION cfoo
 
END MODULE Mod1
!*
PROGRAM Namelist09
      USE MOD1
      IMPLICIT NONE
 
       TYPE(Base(4,5))        :: b1 
       TYPE(Child(4,5,4,5))    :: c1 
       TYPE(NextGen(4,5,4,5,1,7)):: n1 
       COMPLEX zB, zC, zN
 
       NAMELIST /NMLb/b1, /NMLc/c1, /NMLn/n1
       NAMELIST /NMLt/b1,c1,n1
       NAMELIST /NMLb/zB, /NMLc/zC, /NMLn/zN
       NAMELIST /NMLt/zB,zC,zN

       zB = b1%cfoo ()
       zC = c1%cfoo ()
       zN = n1%cfoo ()

       print NMLb
       print NMLc
       print NMLn
       print NMLt
 
END PROGRAM Namelist09
