!*  ===================================================================
!*
!*                               DTP - Namelist
!*
!*  DATE                       : November 12, 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : Namelist with Intrinsic IO
!*  SECONDARY FUNCTIONS TESTED : Pointer and pointer target in namelist
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

        INTEGER :: value = 10
      END TYPE Base

      TYPE, EXTENDS(Base) :: Child (k2,k3,l2,l3)
        INTEGER, KIND :: k2, k3
        INTEGER, LEN  :: l2, l3

        CHARACTER(l2+l3) :: name = 'AAA'
        REAL(k1) :: Rarr(l1+l3+l2) = 1.1D0
      END TYPE Child

      TYPE, EXTENDS(Child) :: NextGen (k4,l4)
        INTEGER, KIND :: k4
        INTEGER, LEN  :: l4

        INTEGER(k2)   :: Iarr(l2) = 2.2D0
        CHARACTER(l1+l2) :: abstract = 'BBB'
      END TYPE NextGen

      END MODULE Mod1
!*
      PROGRAM Namelist03
       USE MOD1
       IMPLICIT NONE

       TYPE(Base(4,:)), POINTER    :: b1(:)
       TYPE(Base(4,5)), TARGET     :: btgt
       TYPE(Child(4,3,4,10,4,10)), TARGET  :: c1
       TYPE(NextGen(4,3,4,4,5,5,20,20)), POINTER :: n1

       CHARACTER(100) :: string, msg
       INTEGER :: stat

       NAMELIST /NMLb/b1,btgt, /NMLc/c1, /NMLn/n1
       NAMELIST /NMLt/b1,c1,n1,b1

       string = 'Niels Henrik David Bohr'
       c1%name = string

       ALLOCATE (b1(5), source = btgt)

       WRITE(*, NML=NMLb, iostat = stat, iomsg = msg)
       WRITE(*, NML=NMLc, iostat = stat, iomsg = msg)

       ALLOCATE (n1, source =  NextGen(4,3,4,4,5,5,20,20) ( value = 1, Iarr = 1.5, Rarr = 3.5, &
          & name = 'Erwin Rudolf Josef Alexander Schrodinger', abstract = 'quantum-photon-Negentropy' ) )

       WRITE(*, NML=NMLn, iostat = stat, iomsg = msg)

       WRITE(*, NML=NMLt, iostat = stat, iomsg = msg)

      END PROGRAM Namelist03
