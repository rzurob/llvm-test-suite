!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : F2003/dtparam/procPtr/
! PROGRAMMER                 : Morteza Ershad-Manesh
! DATE                       : 08/11/2008
! PRIMARY FUNCTIONS TESTED   : procedure declaration statement & procedure component 
! DRIVER STANZA              : xlfF2003
! DESCRIPTION                : Mix use of  procedure declaration statement & procedure component and Save attribute. We define the procedure decl. in two different part of the program.
!                                    We are in a 2D environmnet, dropping an object down at a certain height then we want to find few properties from it.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012
MODULE M
	PROCEDURE(iGETTime), POINTER,SAVE :: GETTimeptr=>null()
	PROCEDURE(iGETMass), POINTER,SAVE :: GETMassptr=>null()
	PROCEDURE(iGETVolume), POINTER,SAVE :: GETVolumeptr=>null()
	PROCEDURE(iGETSurface), POINTER,SAVE :: GETSurfaceptr=>null()	
	
	
    TYPE PropHolder 
     REAL(KIND=4) :: GRAVITY=9.8	
	 REAL(KIND=4) :: pi=3.14
	END TYPE PropHolder
	
    TYPE, ABSTRACT :: OBJECT (k1,LEN1)
	  INTEGER, KIND :: k1
	  INTEGER, LEN :: LEN1
	  CHARACTER(LEN=LEN1)      :: NAME='Object'
	  REAL(kind=k1),PRIVATE,DIMENSION(2) :: positn=(/0.0,0.0/)
	  REAL(kind=k1),PRIVATE :: Mass
	END TYPE OBJECT

	TYPE,EXTENDS(OBJECT),ABSTRACT :: DRAWABLE_OBJ
	 REAL(Kind=k1)  :: r
	CONTAINS
      PROCEDURE(PREPARE_OBJ),PASS(OBJ),DEFERRED :: Get_Properties
	END TYPE DRAWABLE_OBJ

   TYPE,EXTENDS(DRAWABLE_OBJ):: SPHERE
     REAL(kind=k1)    :: Volume=0,Surface=0,Distance=0,Speed=0,Velocity=0,Time=0,TimRes=0
    CONTAINS
     PROCEDURE,PASS(OBJ) :: Get_Properties=>PRINT_SPHERE_PROPERTIES 
	 PROCEDURE,PASS(OBJ) :: Crt_Properties=>Create_SPHERE_PROPERTIES
	 PROCEDURE,PASS(OBJ) :: getDist_Properties=>GetDst_SPHERE_PROPERTIES
	 PROCEDURE,PASS(OBJ) :: CalcVol_Properties=>CalcVol_PROP
	
   END TYPE
 
   ABSTRACT INTERFACE
	 SUBROUTINE PREPARE_OBJ(OBJ)
 	  import DRAWABLE_OBJ
	   CLASS(DRAWABLE_OBJ(4,*)),INTENT(IN) :: OBJ
	 END SUBROUTINE PREPARE_OBJ
	 
	 REAL FUNCTION ICalcVol_PROP(OBJ)
	  import SPHERE
	 CLASS(SPHERE(4,*)),INTENT(IN) ::OBJ
	 END FUNCTION
	 
	REAL FUNCTION iGETTime (OBJ)
	  import SPHERE
	 CLASS(SPHERE(4,*)),INTENT(IN) ::OBJ
	END FUNCTION iGETTime 

	REAL FUNCTION iGETMass (OBJ)
	  import SPHERE
	 CLASS(SPHERE(4,*)),INTENT(IN) ::OBJ
	END FUNCTION iGETMass 
		
    REAL FUNCTION iGETVolume (OBJ)
	  import SPHERE
	 CLASS(SPHERE(4,*)),INTENT(IN) ::OBJ
	END FUNCTION iGETVolume
	
    REAL FUNCTION iGETSurface (OBJ)
	  import SPHERE
	 CLASS(SPHERE(4,*)),INTENT(IN) ::OBJ
	END FUNCTION iGETSurface
	
   END INTERFACE

   CONTAINS
   REAL FUNCTION GetDst_SPHERE_PROPERTIES (OBJ)
     CLASS(SPHERE(4,*)),INTENT(IN) ::OBJ
	 GetDst_SPHERE_PROPERTIES=OBJ%Distance
   END FUNCTION

   REAL FUNCTION CalcVol_PROP(OBJ)
	 CLASS(SPHERE(4,*)),INTENT(IN) ::OBJ
	 TYPE(PropHolder) :: BasicProp
	 
	 SELECT TYPE (OBJ)
	  TYPE IS (SPHERE(4,*))
	   CalcVol_PROP=(4/3)*(BasicProp%pi)*((OBJ%r)**3)
	  CLASS DEFAULT
	   CalcVol_PROP=-20.0
	 END SELECT 
   END FUNCTION
   
 SUBROUTINE PRINT_SPHERE_PROPERTIES (OBJ)
    CLASS(SPHERE(4,*)),INTENT(IN) ::OBJ
	print*,"SPHERE's Property:"
    print*,"SPHERE name: ",TRIM(OBJ%NAME)
	write(*,20) "SPHERE Position:",OBJ%positn
	20      format (1x,1a,t20,/,3f10.2)   
	write(*,20) "SPHERE Radius:",OBJ%r
	write(*,20) "SPHERE Mass:",GETMassptr(OBJ)
	!OBJ%Mass
	write(*,20) "SPHERE Volume:",GETVolumeptr(OBJ)
	!OBJ%Volume
	write(*,20) "SPHERE Surface:",GETSurfaceptr(OBJ)
	!OBJ%Surface
	write(*,20) "SPHERE Time Required to reach position  ~0:",GETTimeptr(OBJ)
	!OBJ%Time
	write(*,20) "SPHERE Speed :",OBJ%Speed
	write(*,20) "SPHERE Velocity :",OBJ%Velocity
 END SUBROUTINE PRINT_SPHERE_PROPERTIES
 
 SUBROUTINE Create_SPHERE_PROPERTIES (OBJ,R,Postn,Mass)
    CLASS(SPHERE(4,*)),INTENT(OUT) ::OBJ
	TYPE(PropHolder) :: BasicProp
	PROCEDURE(ICalcVol_PROP), POINTER :: proptr=>NULL()
	REAL(KIND=4) ::temp1,temp2
	REAL(KIND=4),INTENT(IN) ::R,Mass
	REAL(KIND=4),INTENT(IN),DIMENSION(2) ::Postn
	proptr=>CalcVol_PROP
    OBJ%Name="MySPHERE"
    OBJ%r=R
	temp1=proptr(OBJ) 
	temp2=OBJ%CalcVol_Properties() 
	IF ( temp1 .EQ. temp2 ) THEN
	 OBJ%Volume=OBJ%CalcVol_Properties()
	ELSE
	 STOP 1
	END IF
	OBJ%Surface=4*(BasicProp%pi)*(OBJ%r**2)
	OBJ%positn=(/0.0,30.0/)
	OBJ%Mass=Mass
 END SUBROUTINE Create_SPHERE_PROPERTIES
 
 SUBROUTINE Gen_PROPERTIES (OBJ)
    TYPE(SPHERE(4,*)),INTENT(INOUT) ::OBJ
	TYPE(PropHolder) :: BasicProp
    	
    OBJ%Distance=OBJ%Positn(2)
	OBJ%Time= SQRT((OBJ%Positn(2))/(0.5*BasicProp%GRAVITY))
	OBJ%Speed=(OBJ%getDist_Properties())/OBJ%Time
	OBJ%Velocity=(BasicProp%GRAVITY)*OBJ%Time
 END SUBROUTINE Gen_PROPERTIES
 
 REAL FUNCTION GETTime (OBJ)
   CLASS(SPHERE(4,*)),INTENT(IN) ::OBJ
   GETTime=OBJ%Time
 END FUNCTION GETTime 

 REAL FUNCTION GETMass (OBJ)
  CLASS(SPHERE(4,*)),INTENT(IN) ::OBJ
  GETMass=OBJ%Mass
 END FUNCTION GETMass 
		
 REAL FUNCTION GETVolume (OBJ)
  CLASS(SPHERE(4,*)),INTENT(IN) ::OBJ
  GETVolume=OBJ%Volume
 END FUNCTION GETVolume
	
 REAL FUNCTION GETSurface (OBJ)
  CLASS(SPHERE(4,*)),INTENT(IN) ::OBJ
  GETSurface=OBJ%Surface
 END FUNCTION GETSurface
	

END MODULE


PROGRAM procdeclfn15
USE M
 TYPE(SPHERE(4,20)) :: mysphere
 REAL(KIND=4) ::R,Mass
 REAL(KIND=4),DIMENSION(2) ::Postn
    GETTimeptr=>GETTime
	GETMassptr=>GETMass
	GETVolumeptr=>GETVolume
	GETSurfaceptr=>GETSurface
	
	Postn=(/10,30/)
	R=4
	Mass=1.0
	
    call mysphere%Crt_Properties(R,Postn,Mass)
    call Gen_PROPERTIES(mysphere)
    call mysphere%Get_Properties()
	
END PROGRAM procdeclfn15