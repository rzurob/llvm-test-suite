!*  ===================================================================
!*
!*                               DTP - Namelist
!*
!*  DATE                       : November 26, 2008
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
!234567890123456789012345678901234567890123456789012345678901234567890
MODULE Mod1
      IMPLICIT NONE

      TYPE Base (k1,l1)  ! (1,1)
        INTEGER, KIND :: k1
        INTEGER, LEN  :: l1

        CHARACTER(l1) :: Carr(l1) = 'ABC'

        CONTAINS
          PROCEDURE, NOPASS :: MyTypbd
      END TYPE Base

      TYPE, EXTENDS(Base) :: Child (k2,k3,l2,l3) ! (2,2,2,2)
        INTEGER(k1), KIND :: k2, k3
        INTEGER(k1), LEN  :: l2, l3

        INTEGER(k2+k3) :: Iarr(l1) = k1
        REAL(k2+k3)    :: Rarr(l3) = k3
      END TYPE Child

      TYPE, EXTENDS(Child) :: NextGen (k4,l4)   !(1024,1024)
        INTEGER(k1), KIND :: k4
        INTEGER(k1), LEN  :: l4

        LOGICAL(k1)    :: Bool(l2) = .FALSE.
        COMPLEX(k2+k3) :: Zarr(l2) = (k2,k3)
      END TYPE NextGen

      SAVE

      TYPE(Base(1,1))        :: b1 = Base(1,1) ('Erwin')
      TYPE(Child(1,1,2,2,2,2))  :: c1
      TYPE(NextGen(1,1,2,2,2,2,1024,1024)), PRIVATE :: n1

      NAMELIST /NML1/b1,c1,n1, /NML2/b1
      NAMELIST /NML2/b1, /NML3/c1,c1

      PRIVATE NML1

      CONTAINS

      SUBROUTINE MyTypbd

        WRITE(*, NML=NML1)
        WRITE(*, NML=NML2)
        WRITE(*, NML=NML3)

      END SUBROUTINE

END MODULE Mod1
!*
PROGRAM Namelist12
       USE MOD1, ONLY: b1, NML2, Base
       IMPLICIT NONE

       CALL b1%MyTypbd()

       b1 = Base(1,1) ('Niels')

       WRITE(*, NML=NML2)

END PROGRAM Namelist12
