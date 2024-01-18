!*  ===================================================================
!*
!*                               DTP - Namelist
!*
!*  DATE                       : November 17, 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : Namelist with Intrinsic IO
!*  SECONDARY FUNCTIONS TESTED : Child2 encloses Child1
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

        INTEGER(k1) :: val = k1
      END TYPE Base

      TYPE, EXTENDS(Base) :: Child1 (k2,k3,l2,l3)
        INTEGER, KIND :: k2, k3
        INTEGER, LEN  :: l2, l3

        INTEGER(k1)   :: Iarr(l1/l1,l2/l3,l3/l2) = 0
        CHARACTER(l1) :: name = 'ABCDEFG'
      END TYPE Child1

      TYPE, EXTENDS(Base) :: Child2 (k4,l4)
        INTEGER, KIND :: k4
        INTEGER, LEN  :: l4

        TYPE(Child1(k1,l1,k4,k4,l4,l4)) :: cmp
      END TYPE Child2

      END MODULE Mod1
!*
      PROGRAM Namelist05
       USE MOD1
       IMPLICIT NONE

       TYPE(Base(4,:)), POINTER                :: b1(:)
       TYPE(Base(4,5)), DIMENSION(10), TARGET  :: btgt
       TYPE(Child1(4,10,100,100,100,100))      :: c1
       TYPE(Child2(4,10,100,100)), ALLOCATABLE :: n1

       CHARACTER(100) :: string, msg
       INTEGER :: stat

       NAMELIST /NMLb/b1, /NMLc/c1, /NMLn/n1
       NAMELIST /NMLt/b1,c1,n1,btgt

       ALLOCATE (Base(4,5) :: b1(2))
       b1(3:4) => btgt

       WRITE(*, NML=NMLb, iostat = stat, iomsg = msg)
       WRITE(*, NML=NMLc, iostat = stat, iomsg = msg)

       ALLOCATE (n1, source =  Child2(4,10,100,100) (val =1, &
          & cmp = Child1(4,10,100,100,100,100) (Iarr = 1.9, name = 'Erwin Rudolf Josef Alexander Schrodinger') ) )

       WRITE(*, NML=NMLn, iostat = stat, iomsg = msg)

       string = 'Niels Henrik David Bohr'
       n1%cmp%name = string (7:15)

       WRITE(*, NML=NMLt, iostat = stat, iomsg = msg)

       n1%cmp%name = 'Erwin Rudolf Josef Alexander Schrodinger'

       print NMLt
       print NMLn
       print NMLc
       print NMLb

      END PROGRAM Namelist05
