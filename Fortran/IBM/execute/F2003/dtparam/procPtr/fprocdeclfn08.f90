!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : F2003/dtparam/procPtr/
! PROGRAMMER                 : Morteza Ershad-Manesh
! DATE                       : 08/05/2008
! PRIMARY FUNCTIONS TESTED   : procedure declaration statement & procedure component 
! DRIVER STANZA              : xlfF2003
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
      PROCEDURE(PREPARE_OBJ),PASS(OBJ),DEFERRED :: Get_Properties
	END TYPE DRAWABLE_OBJ

   TYPE,EXTENDS(DRAWABLE_OBJ):: CUBE
     REAL(kind=k1)    :: Volume=0,Surface=0
    CONTAINS
     PROCEDURE,PASS(OBJ) :: Get_Properties=>GET_CUBE_PROPERTIES
	 PROCEDURE,PASS(OBJ) :: Set_Properties=>SET_CUBE_PROPERTIES
   END TYPE
	
	
	ABSTRACT INTERFACE
	 SUBROUTINE PREPARE_OBJ(OBJ)
	 import DRAWABLE_OBJ
	  CLASS(DRAWABLE_OBJ(4,*)),INTENT(IN) :: OBJ
	 END SUBROUTINE PREPARE_OBJ
	 
	 SUBROUTINE PREPARE_OBJ2(OBJ,OBJ2)
	 import DRAWABLE_OBJ
	  CLASS(DRAWABLE_OBJ(4,*)),INTENT(INOUT) :: OBJ,OBJ2
	 END SUBROUTINE PREPARE_OBJ2
 
 	 REAL FUNCTION ICUBE_SURFACEAREA_OBJ(OBJ)
	 import CUBE
	  TYPE(CUBE(4,20)),INTENT(IN) :: OBJ
	 END FUNCTION ICUBE_SURFACEAREA_OBJ
	 
	 REAL FUNCTION ICUBE_Volume_OBJ(OBJ)
	 import CUBE
	  TYPE(CUBE(4,20)),INTENT(IN) :: OBJ
	 END FUNCTION ICUBE_Volume_OBJ
    END INTERFACE	

 CONTAINS
  SUBROUTINE GET_CUBE_PROPERTIES (OBJ)
    CLASS(CUBE(4,*)),INTENT(IN) ::OBJ
	print*,"CUBE's Property:"
    print*,"Cube name: ",TRIM(OBJ%NAME)
	print*,"Cube SIDES LENGHT:",OBJ%SIDES1
	print*,"Cube Volume:",OBJ%Volume
	print*,"Cube Surface:",OBJ%Surface
  END SUBROUTINE GET_CUBE_PROPERTIES
  
  SUBROUTINE SET_CUBE_PROPERTIES (OBJ,OBJ2)
    CLASS(CUBE(4,*)),INTENT(OUT) ::OBJ
	TYPE(CUBE(4,20)),INTENT(IN) ::OBJ2
	PROCEDURE(ICUBE_SURFACEAREA_OBJ),POINTER :: SA =>NULL()
	PROCEDURE(ICUBE_Volume_OBJ),POINTER :: A  =>NULL()
	
	OBJ%NAME=OBJ2%NAME
	OBJ%SIDES1=OBJ2%SIDES1
	SA=>CUBE_SURFACEAREA_OBJ
	OBJ%Surface=SA(OBJ)
	A=>CUBE_Volume_OBJ
	OBJ%Volume=A(OBJ)

  END SUBROUTINE SET_CUBE_PROPERTIES

  REAL FUNCTION CUBE_SURFACEAREA_OBJ(OBJ)
	  TYPE(CUBE(4,20)),INTENT(IN) :: OBJ
	  REAL(KIND=4)             :: RES(1)
	  RES=6*((OBJ%SIDES1)**2)
	  CUBE_SURFACEAREA_OBJ=RES(1)
  END FUNCTION CUBE_SURFACEAREA_OBJ
  
  REAL FUNCTION CUBE_Volume_OBJ(OBJ)
	  TYPE(CUBE(4,20)),INTENT(IN) :: OBJ
	  REAL(KIND=4)             :: RES(1)
	  RES=((OBJ%SIDES1)**3)
	  CUBE_Volume_OBJ=RES(1)
  END FUNCTION CUBE_Volume_OBJ  
  
END MODULE


PROGRAM procdeclfn08
USE M
 TYPE(CUBE(4,20) ),TARGET  :: MyCube,TempCube,TempCube2
 PROCEDURE(), POINTER :: procptr

 
 TempCube%NAME="MyCube"
 TempCube%SIDES1=(/10.0/)
 print*,"Before Setting Values"
 print*,"--------------------"
 call MyCube%Get_Properties()
 call MyCube%Set_Properties(TempCube)
 print*,""
 print*,"After Setting Values"
 print*,"--------------------"
  call MyCube%Get_Properties()
END PROGRAM procdeclfn08

