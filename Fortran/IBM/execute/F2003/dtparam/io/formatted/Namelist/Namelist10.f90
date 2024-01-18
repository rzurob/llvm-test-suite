!*  ===================================================================
!*
!*                               DTP - Namelist
!*
!*  DATE                       : November 26, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Namelist with Intrinsic IO
!*  SECONDARY FUNCTIONS TESTED :
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
!* See defect 353309 and 358772
!234567890123456789012345678901234567890123456789012345678901234567890
MODULE Mod1
      IMPLICIT NONE

      TYPE Base (k1,l1)
        INTEGER, KIND :: k1
        INTEGER, LEN  :: l1

        CHARACTER(l1) :: Carr(l1) = 'ABC'
        INTEGER(k1)   :: Iarr(l1) = 1
      END TYPE Base

      TYPE, EXTENDS(Base) :: Child (k2,k3,l2,l3)
        INTEGER, KIND :: k2, k3
        INTEGER, LEN  :: l2, l3

        REAL(k1) :: Rarr(l3) = 1.0
      END TYPE Child

      TYPE, EXTENDS(Child) :: NextGen (k4,l4)
        INTEGER, KIND :: k4
        INTEGER, LEN  :: l4
      END TYPE NextGen

      CONTAINS

      ELEMENTAL FUNCTION MyAssig(Argb)
        CLASS(Base(4,10)), INTENT(IN) :: Argb
        TYPE(Base(4,10)) :: MyAssig

        MyAssig = Argb

      END FUNCTION

END MODULE Mod1
!*
PROGRAM Namelist10
       USE MOD1
       IMPLICIT NONE

       TYPE(Base(4,:)), ALLOCATABLE :: b0, b1, b2(:)

       NAMELIST /NMLb1/b0,b1, /NMLb2/b2

       ALLOCATE (Base(4,10):: b0, b1, b2(2))

       b0 = Base(4,10) (Carr = 'Niels ', Iarr = 8)
       b1 = MyAssig(Base(4,10) (Carr = 'Niels ', Iarr = 8))

       WRITE(*, NML=NMLb1, DELIM='APOSTROPHE')

       b2 = MyAssig( [NextGen(4,10,4,5,4,5,10,10) ('Erwin',6, 6.6), NextGen(4,10,4,5,4,5,10,10) ('Werner',7, 7.7)] )

       WRITE(*, NML=NMLb2, DELIM='APOSTROPHE')

END PROGRAM Namelist10
