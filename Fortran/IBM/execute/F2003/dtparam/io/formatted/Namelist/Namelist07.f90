!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONMLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : Namelist07
!*                               DTP - Namelist
!*
!*  PROGRAMMER                 : Dorra Bouchiha 
!*  DATE                       : November 17, 2008
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
!234567890123456789012345678901234567890123456789012345678901234567890
      MODULE Mod1
      IMPLICIT NONE 

      TYPE Base (k1,l1)
        INTEGER, KIND :: k1 
        INTEGER, LEN  :: l1 

        INTEGER :: val = 10
      END TYPE Base 

      TYPE, EXTENDS(Base) :: Child2 (k2,l2)
        INTEGER, KIND :: k2
        INTEGER, LEN  :: l2
 
        CHARACTER(l1+l2) :: name = 'ABCDEF'
      END TYPE Child2 

      TYPE, EXTENDS(Base) :: Child3 (k3,l3)
        INTEGER, KIND :: k3
        INTEGER, LEN  :: l3
 
        CHARACTER(l1+l3) :: name = 'GHIJKL'
      END TYPE Child3 

      TYPE, EXTENDS(Child2) :: NextGen (k4,l4)
        INTEGER, KIND :: k4
        INTEGER, LEN  :: l4
 
        TYPE(Child2(k1,l1,k4,l4)) :: c2_cmp(l4)
        TYPE(Child3(k1,l1,k4,l4)) :: c3_cmp(l4)
      END TYPE NextGen

      END MODULE Mod1
!*
      PROGRAM Namelist07
       USE MOD1
       IMPLICIT NONE 
 
       TYPE(Base(4,3)) :: b1 = Base(4,3) (val=-1)
       TYPE(Child2(4,3,4,1)) :: c2 = Child2(4,3,4,1)(val=-2)
       TYPE(Child3(4,3,4,1)) :: c3 = Child3(4,3,4,1)(val=-3)
       TYPE(NextGen(4,3,5,5,4,1)), ALLOCATABLE :: n1(:,:,:,:,:,:,:)

       CHARACTER(100) :: string, msg 
       INTEGER :: stat
 
       NAMELIST /NMLb/b1, /NMLc/c2,c3, /NMLn/n1
       NAMELIST /NMLt/n1,c2,c3,b1

       WRITE(*, NML=NMLb, iostat = stat, iomsg = msg) 
       WRITE(*, NML=NMLc, iostat = stat, iomsg = msg) 

       ALLOCATE(n1(1,1,1,1,1,1,2))

       n1 = NextGen(4,3,5,5,4,1) ( val = 99, c2_cmp = c2, c3_cmp = c3, &
                     & name = 'Erwin Rudolf Josef Alexander Schrodinger' )

       WRITE(*, NML=NMLn, iostat = stat, iomsg = msg) 
 
       string = 'Niels Henrik David Bohr' 
       n1%name = string (7:14)
  
       WRITE(*, NML=NMLt, iostat = stat, iomsg = msg) 
    
       DEALLOCATE(n1) 
 
      END PROGRAM Namelist07
