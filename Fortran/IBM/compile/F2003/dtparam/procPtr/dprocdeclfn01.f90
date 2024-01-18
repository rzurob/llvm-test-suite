!=======================================================================
! TEST BUCKET                : F2003/dtparam/procPtr/
! DATE                       : 08/05/2008
! PRIMARY FUNCTIONS TESTED   : procedure declaration statement & procedure component
! DESCRIPTION                : Use of  procedure declaration statement & procedure component with NoPass and PASS attr. and DEFFERED attr.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

MODULE M

    TYPE, ABSTRACT :: OBJECT (k1,LEN1)
       INTEGER, KIND :: k1
       INTEGER, LEN :: LEN1
       REAL(kind=k1), DIMENSION(1) :: SIDES1=(/0.0/)
       REAL(kind=k1), DIMENSION(2) :: SIDES2=(/0.0,0.0/)
       REAL(kind=k1), DIMENSION(3) :: SIDES3=(/0.0,0.0,0.0/)
       CHARACTER(LEN=LEN1)      :: NAME='Object'
   END TYPE OBJECT

   TYPE,EXTENDS(OBJECT),ABSTRACT :: DRAWABLE_OBJ
       REAL, DIMENSION(3)   :: COLOR=(/0.0,0.0,0.0/)
   CONTAINS
       PROCEDURE,PASS(OBJ),DEFERRED :: Get_Properties
   END TYPE DRAWABLE_OBJ

   TYPE,EXTENDS(DRAWABLE_OBJ):: CUBE
       REAL(kind=k1)    :: Volume=0,Surface=0
  CONTAINS
       PROCEDURE,PASS(OBJ) :: Get_Properties=>GET_CUBE_PROPERTIES
       PROCEDURE,PASS(OBJ) :: Set_Properties=>SET_CUBE_PROPERTIES
   END TYPE

 CONTAINS
  SUBROUTINE GET_CUBE_PROPERTIES (OBJ)
    CLASS(CUBE(4,*)),INTENT(IN) ::OBJ
  END SUBROUTINE GET_CUBE_PROPERTIES

  SUBROUTINE SET_CUBE_PROPERTIES (OBJ,OBJ2)
    CLASS(CUBE(4,*)),INTENT(OUT) ::OBJ
    TYPE(CUBE(4,20)),INTENT(IN) ::OBJ2
  END SUBROUTINE SET_CUBE_PROPERTIES

  REAL FUNCTION CUBE_SURFACEAREA_OBJ(OBJ)
    TYPE(CUBE(4,20)),INTENT(IN) :: OBJ
    REAL(KIND=4)             :: RES(1)
    CUBE_SURFACEAREA_OBJ=10.0
  END FUNCTION CUBE_SURFACEAREA_OBJ

  REAL FUNCTION CUBE_Volume_OBJ(OBJ)
    TYPE(CUBE(4,20)),INTENT(IN) :: OBJ
    REAL(KIND=4)             :: RES(1)
    CUBE_Volume_OBJ=10.2
  END FUNCTION CUBE_Volume_OBJ
END MODULE


PROGRAM procdeclfn01
END PROGRAM procdeclfn01

