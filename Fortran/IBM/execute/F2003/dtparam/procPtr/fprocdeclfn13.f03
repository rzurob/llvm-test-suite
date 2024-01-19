!=======================================================================
! TEST BUCKET                : F2003/dtparam/procPtr/
! DATE                       : 08/05/2008
! PRIMARY FUNCTIONS TESTED   : procedure declaration statement & procedure component
! DESCRIPTION                : Mix user of  procedure declaration statement & procedure component with passed attr.
!                                    We are in a 2D environmnet, dropping an object down at a certain height then we want to find few properties from it.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

MODULE M

    TYPE PropHolder
     REAL(KIND=4) :: GRAVITY=9.8

	END TYPE PropHolder

    TYPE, ABSTRACT :: OBJECT (k1,LEN1)
	  INTEGER, KIND :: k1
	  INTEGER, LEN :: LEN1
	  CHARACTER(LEN=LEN1)      :: NAME='Object'
	  REAL(KIND=k1) :: slope,b
	  REAL(kind=k1),PRIVATE,DIMENSION(2) :: positn=(/0.0,0.0/)
	  REAL(kind=k1),PRIVATE :: Mass
	END TYPE OBJECT

	TYPE,EXTENDS(OBJECT) :: POINT
	END TYPE POINT

	TYPE,EXTENDS(OBJECT),ABSTRACT :: DRAWABLE_OBJ
	 REAL(Kind=k1)  :: r
	CONTAINS
      PROCEDURE(PREPARE_OBJ),PASS(OBJ),DEFERRED :: Get_Properties
	END TYPE DRAWABLE_OBJ

   TYPE,EXTENDS(DRAWABLE_OBJ):: SPHERE
     REAL(kind=k1)    :: Volume=0,Surface=0,Distance=0,Speed=0,Velocity=0,Time=0,TimRes=0,Acceleration=0,Force=0,AngleOFincRay=0
	PROCEDURE(ICalculateLinefrom),POINTER :: CalcLinefrom_Properties=>NULL()
    CONTAINS

     PROCEDURE,PASS(OBJ) :: Get_Properties=>PRINT_SPHERE_PROPERTIES
	 PROCEDURE,PASS(OBJ) :: Crt_Properties=>Create_SPHERE_PROPERTIES
	 PROCEDURE,PASS(OBJ) :: getDist_Properties=>GetDst_SPHERE_PROPERTIES
	 PROCEDURE,PASS(OBJ) :: CalcVol_Properties=>CalcVol_PROP
	 PROCEDURE,PASS(OBJ) :: CalcAcc_Properties=>CalcACC_PROP
	 PROCEDURE,PASS(OBJ) :: CalcFcc_Properties=>CalcFRC_PROP
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

	  SUBROUTINE ICalcACC_PROP(OBJ)
	  import SPHERE
	 CLASS(SPHERE(4,*)),INTENT(INOUT) ::OBJ
	 END SUBROUTINE ICalcACC_PROP

	 SUBROUTINE ICalculateLinefrom(OBJ,intPos)
	 import SPHERE
	 import POINT
     TYPE(POINT(4,20)),INTENT(IN) :: intPos
	 CLASS(SPHERE(4,*)),INTENT(INOUT) ::OBJ
	 END SUBROUTINE ICalculateLinefrom

   END INTERFACE

   CONTAINS
   REAL FUNCTION GetDst_SPHERE_PROPERTIES (OBJ)
     CLASS(SPHERE(4,*)),INTENT(IN) ::OBJ
	 GetDst_SPHERE_PROPERTIES=OBJ%Distance
   END FUNCTION

   REAL FUNCTION CalcVol_PROP(OBJ)
	 CLASS(SPHERE(4,*)),INTENT(IN) ::OBJ

	 SELECT TYPE (OBJ)
	  TYPE IS (SPHERE(4,*))
	   CalcVol_PROP=(4/3)*(3.1415)*((OBJ%r)**3)
	  CLASS DEFAULT
	   CalcVol_PROP=-20.0
	 END SELECT
   END FUNCTION

   SUBROUTINE  CalcACC_PROP(OBJ)
	 CLASS(SPHERE(4,*)),INTENT(INOUT) ::OBJ
	 OBJ%Acceleration=(OBJ%Velocity)/(OBJ%Time)
   END SUBROUTINE CalcACC_PROP

   SUBROUTINE  CalcFRC_PROP(OBJ)
	 CLASS(SPHERE(4,*)),INTENT(INOUT) ::OBJ
	 OBJ%Force=(OBJ%Mass)*(OBJ%Acceleration)
   END SUBROUTINE CalcFRC_PROP

 SUBROUTINE PRINT_SPHERE_PROPERTIES (OBJ)
    CLASS(SPHERE(4,*)),INTENT(IN) ::OBJ
	print*,"SPHERE's Property:"
    print*,"SPHERE name: ",TRIM(OBJ%NAME)
	write(*,20) "SPHERE Position:",OBJ%positn
	20      format (1x,1a,t20,/,3f10.2)
	21      format (1x,1a,1f5.2,1a)
	write(*,20) "SPHERE Radius:",OBJ%r
	write(*,20) "SPHERE Mass:",OBJ%Mass
	write(*,20) "SPHERE Volume:",OBJ%Volume
	write(*,20) "SPHERE Surface:",OBJ%Surface
	write(*,20) "SPHERE Time Required to reach position  ~0:",OBJ%Time
	write(*,20) "SPHERE Speed :",OBJ%Speed
	write(*,20) "SPHERE Velocity :",OBJ%Velocity
	write(*,20) "SPHERE Acceleration :",OBJ%Acceleration
	write(*,20) "SPHERE Force (F=ma) :",OBJ%Force
	write(*,21) "In order to see the reflection of the sphere must be looking at an angle of ",OBJ%AngleOFincRay , " degrees"
 END SUBROUTINE PRINT_SPHERE_PROPERTIES

 SUBROUTINE Create_SPHERE_PROPERTIES (OBJ,R,Postn,Mass,name)
    CLASS(SPHERE(4,*)),INTENT(OUT) ::OBJ
	PROCEDURE(ICalcVol_PROP), POINTER :: proptr=>NULL()
	REAL(KIND=4) ::temp1,temp2
	CHARACTER(LEN=20),INTENT(IN) :: name
	REAL(KIND=4),INTENT(IN) ::R,Mass
	REAL(KIND=4),INTENT(IN),DIMENSION(2) ::Postn
	proptr=>CalcVol_PROP
    OBJ%Name=TRIM(name)
    OBJ%r=R
	temp1=proptr(OBJ)
	temp2=OBJ%CalcVol_Properties()
	IF ( temp1 .EQ. temp2 ) THEN
	 OBJ%Volume=OBJ%CalcVol_Properties()
	ELSE
	 STOP 1
	END IF
	OBJ%Surface=4*(3.1415)*(OBJ%r**2)
	OBJ%positn=Postn
	OBJ%Mass=Mass
 END SUBROUTINE Create_SPHERE_PROPERTIES

 SUBROUTINE Gen_PROPERTIES (OBJ)
    TYPE(SPHERE(4,*)),INTENT(INOUT) ::OBJ
    OBJ%Distance=OBJ%Positn(2)
	OBJ%Time= SQRT((OBJ%Positn(2))/(0.5*9.8))
	OBJ%Speed=(OBJ%getDist_Properties())/OBJ%Time
	OBJ%Velocity=(9.8)*OBJ%Time
	call OBJ%CalcAcc_Properties()
	call OBJ%CalcFcc_Properties()
 END SUBROUTINE Gen_PROPERTIES


 SUBROUTINE CalculateLinefrom(OBJ,intPos)
      TYPE(POINT(4,20)),INTENT(IN) :: intPos
	  CLASS(SPHERE(4,*)),INTENT(INOUT) ::OBJ
	  PROCEDURE(REAL), POINTER :: procptr=>NULL()
	  REAL(KIND=4) :: pos1=0,temp=0,temp2=0
	  OBJ%Slope=(OBJ%positn(2)-intPos%positn(2))/(OBJ%positn(1)-intPos%positn(1))
	  OBJ%b = 0
	  temp=(OBJ%positn(1)/OBJ%positn(2))
	  procptr=>ATAN
	  temp2=(180*procptr(temp))/3.1415
	  !Assuming we are going from ICE to Water
	  temp=(1.333*sin(temp2))/1.31
	  procptr=>ASIN
	  OBJ%AngleOFincRay = (180*procptr(temp))/3.1415
	  print*,(180*procptr(temp))/3.1415

 END SUBROUTINE CalculateLinefrom


END MODULE


PROGRAM procdeclfn13
USE M

 TYPE(SPHERE(4,20)) :: mysphere
 TYPE(POINT(4,20)) :: origin
 CHARACTER(LEN=20) :: NAM="Sphere1"
 REAL(KIND=4) ::R,Mass,temp
 REAL(KIND=4),DIMENSION(2) ::Postn
 PROCEDURE(ICalcVol_PROP),POINTER ::prcptr
	Postn=(/10,30/)
	R=4
	Mass=2.4
 call mysphere%Crt_Properties(R,Postn,Mass,NAM)
 call Gen_PROPERTIES(mysphere)
 print*,""
 	Postn=(/1,550/)
	R=10
	Mass=700
	NAM="Sphere2"

  print*,"Direct Call"
  temp=CalcVol_PROP(mysphere)
 	Postn=(/1.0,temp/)
	R=6
	Mass=5
	NAM="Sphere3"
 call mysphere%Crt_Properties(R,Postn,Mass,NAM)
 call Gen_PROPERTIES(mysphere)
  mysphere%CalcLinefrom_Properties=>CalculateLinefrom
 call mysphere%CalcLinefrom_Properties(origin)
 call mysphere%Get_Properties()
   print*,""
  print*,"Call using procptr"
 prcptr=>CalcVol_PROP
	Postn=(/prcptr(mysphere)*2,1.0/)
	R=6
	Mass=5
	NAM="Sphere4"
 call mysphere%Crt_Properties(R,Postn,Mass,NAM)
 call Gen_PROPERTIES(mysphere)
  	Postn=(/0.0,0.0/)
 mysphere%CalcLinefrom_Properties=>CalculateLinefrom
 call mysphere%CalcLinefrom_Properties(origin)
 call mysphere%Get_Properties()

END PROGRAM procdeclfn13