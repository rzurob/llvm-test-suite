!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONMLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : Namelist02
!*                               DTP - Namelist
!*
!*  PROGRAMMER                 : Dorra Bouchiha 
!*  DATE                       : November 12, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Namelist with Intrinsic IO
!*  SECONDARY FUNCTIONS TESTED : Structure constructor 
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

        CHARACTER(l2+l3) :: name = 'AAA'     
        REAL(k1) :: rarr(l1+l3-l2) = 1.1D0
      END TYPE Child 

      TYPE, EXTENDS(Child) :: NextGen (k4,l4)
        INTEGER, KIND :: k4
        INTEGER, LEN  :: l4
 
        INTEGER(k2)   :: iarr(l2) = 2.2D0 
        CHARACTER(l1+l2) :: abstract = 'BBB'
      END TYPE NextGen

      END MODULE Mod1
!*
      PROGRAM Namelist02
       USE MOD1
       IMPLICIT NONE 
 
       TYPE(Base(4,3))        :: b1(5)
       TYPE(Child(4,3,4,10,4,10))    :: c1
       TYPE(NextGen(4,3,4,4,5,5,20,20)) :: n1

       CHARACTER(100) :: string, msg 
       INTEGER :: stat
 
       NAMELIST /NMLb/b1, /NMLc/c1, /NMLn/n1
       NAMELIST /NMLt/b1,c1,n1

       string = 'Niels Henrik David Bohr' 
       n1%abstract = 'electron-orbits-atom-nucleus'
       c1%name = string 

       WRITE(*, NML=NMLb, iostat = stat, iomsg = msg) 
       WRITE(*, NML=NMLc, iostat = stat, iomsg = msg) 
       WRITE(*, NML=NMLn, iostat = stat, iomsg = msg) 

       n1 = NextGen(4,3,4,4,5,5,20,20) ( value = 1, iarr = 1.5, rarr = 3.5, &
          & name = 'Erwin Rudolf Josef Alexander Schrodinger', abstract = 'quantum-photon-Negentropy' )

       WRITE(*, NML=NMLt, iostat = stat, iomsg = msg) 
 
      END PROGRAM Namelist02
