!*  ===================================================================
!*
!*                               DTP - Namelist
!*
!*  DATE                       : November 17, 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : Namelist with Intrinsic IO
!*  SECONDARY FUNCTIONS TESTED : child type encloses parent type
!*
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

      TYPE, EXTENDS(Base) :: Child (k2,k3,l2,l3)
        INTEGER, KIND :: k2, k3
        INTEGER, LEN  :: l2, l3

        TYPE(Base(k2,l2+l3-1)) :: cmp
      END TYPE Child

      TYPE, EXTENDS(Child) :: NextGen (k4,l4)
        INTEGER, KIND :: k4
        INTEGER, LEN  :: l4

        INTEGER(k2)   :: Iarr(l2) = 2.2D0
        CHARACTER(l1+l2) :: name = 'BBB'
      END TYPE NextGen

      END MODULE Mod1
!*
      PROGRAM Namelist04
       USE MOD1
       IMPLICIT NONE

       TYPE(Base(4,:)), ALLOCATABLE :: b1(:)
       TYPE(Child(4,3,4,10,4,10)) :: c1
       TYPE(NextGen(4,3,4,4,5,5,20,20)), ALLOCATABLE :: n1

       CHARACTER(100) :: string, msg
       INTEGER :: stat

       NAMELIST /NMLb/b1, /NMLc/c1, /NMLn/n1
       NAMELIST /NMLt/b1,c1,n1,b1

       ALLOCATE (Base(4,10) :: b1(-5:-3))

       WRITE(*, NML=NMLb, iostat = stat, iomsg = msg)
       WRITE(*, NML=NMLc, iostat = stat, iomsg = msg)

       ALLOCATE (n1, source =  NextGen(4,3,4,4,5,5,20,20) ( val = 1, cmp = Base(4,9) (val = 99) , Iarr = 1.5, &
          & name = 'Erwin Rudolf Josef Alexander Schrodinger' ) )

       WRITE(*, NML=NMLn, iostat = stat, iomsg = msg)

       string = 'Niels Henrik David Bohr'
       n1%name = string (7:15)

       WRITE(*, NML=NMLt, iostat = stat, iomsg = msg)

       DEALLOCATE(b1,n1)

      END PROGRAM Namelist04
