!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONMLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : Namelist01
!*                               DTP - Namelist
!*
!*  PROGRAMMER                 : Dorra Bouchiha 
!*  DATE                       : November 12, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Namelist with Intrinsic IO
!*  SECONDARY FUNCTIONS TESTED :
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

      TYPE Base (k1,l1)
        INTEGER, KIND :: k1 
        INTEGER, LEN  :: l1 

        INTEGER :: value = 10
      END TYPE Base 

      TYPE, EXTENDS(Base) :: Child (k2,k3,l2,l3)
        INTEGER, KIND :: k2, k3
        INTEGER, LEN  :: l2, l3

        REAL(k1) :: R(l1+l3) = 1.0 
      END TYPE Child 

      TYPE, EXTENDS(Child) :: NextGen (k4,l4)
        INTEGER, KIND :: k4
        INTEGER, LEN  :: l4
 
        CHARACTER(l1+l2+l3+l4) :: C = 'ABC'
        INTEGER(k2)   :: I(l1+l2) = 2.0 
      END TYPE NextGen

      END MODULE Mod1
!*
      PROGRAM Namelist01
       USE MOD1
 
       TYPE(Base(4,3))        :: b1
       TYPE(Child(4,3,4,5,4,5))    :: c1
       TYPE(NextGen(4,3,4,4,10,10,20,20)):: n1
 
       NAMELIST /NMLb/b1, /NMLc/c1, /NMLn/n1
       NAMELIST /NMLt/b1,c1,n1
 
       print NMLb
       print NMLc
       print NMLn
       print NMLt
 
      END PROGRAM Namelist01
