!*  ===================================================================
!*
!*                               DTP - Namelist
!*
!*  DATE                       : November 28, 2008
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

        PRIVATE
        LOGICAL(k1)    :: Bool(l2) = .FALSE.
        COMPLEX(k2+k3) :: Zarr(l2) = (k2,k3)
      END TYPE NextGen

      SAVE

      TYPE(Base(1,1))        :: b1 = Base(1,1) ('Erwin')
      TYPE(Child(1,1,2,2,2,2))  :: c1
      TYPE(NextGen(1,1,2,2,2,2,1024,1024)), PUBLIC :: n1

      NAMELIST /NML1/b1,c1,n1
      NAMELIST /NML2/b1
      NAMELIST /NML2/b1, /NML3/c1,c1

      CONTAINS

      SUBROUTINE MyTypbd

       READ(10, NML=NML1)
       READ(10, NML=NML2)
       READ(10, NML=NML3)

       IF (b1%Carr(1) .NE. 'E') ERROR STOP 20
       IF (c1%Carr(1) .NE. 'A') ERROR STOP 21
       IF (ANY(c1%Iarr .NE. 1)) ERROR STOP 22
       IF (ANY(c1%Rarr .NE. 2.0)) ERROR STOP 23
       IF (n1%Carr(1) .NE. 'A') ERROR STOP 24
       IF (ANY(n1%Iarr .NE. 1)) ERROR STOP 25
       IF (ANY(n1%Rarr .NE. 2.0)) ERROR STOP 26
       IF (ANY(n1%Bool)) ERROR STOP 27
       IF (ANY(REAL(n1%Zarr) .NE. 2.0)) ERROR STOP 28
       IF (ANY(IMAG(n1%Zarr) .NE. 2.0)) ERROR STOP 29

      END SUBROUTINE

END MODULE Mod1
!*
PROGRAM Namelist13
       USE MOD1
       IMPLICIT NONE

       OPEN (10, file = 'Namelist13.In', form='formatted')

       CALL n1%MyTypbd()

       READ(10, NML=NML2)
       READ(10, NML=NML3)

       IF (b1%Carr(1) .NE. 'N') ERROR STOP 10
       IF (c1%Carr(1) .NE. 'N') ERROR STOP 11
       IF (ANY(c1%Iarr .NE. 8)) ERROR STOP 12
       IF (ANY(c1%Rarr .NE. 8.8)) ERROR STOP 13

END PROGRAM Namelist13

