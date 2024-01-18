!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : F2003/dtparam/procPtr/
! PROGRAMMER                 : Morteza Ershad-Manesh
! DATE                       : 08/05/2008
! PRIMARY FUNCTIONS TESTED   : procedure declaration statement & procedure component 
! DRIVER STANZA              : xlfF2003
! DESCRIPTION                : Use of  procedure declaration statement & procedure component with passed attr.  (Variable types of Private)
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
	write(*,20) "SPHERE Mass:",OBJ%Mass
	write(*,20) "SPHERE Volume:",OBJ%Volume
	write(*,20) "SPHERE Surface:",OBJ%Surface
	write(*,20) "SPHERE Time Required to reach position  ~0:",OBJ%Time
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

END MODULE


PROGRAM procdeclfn14
USE M

 TYPE(SPHERE(4,20)) :: mysphere
 REAL(KIND=4) ::R,Mass
 REAL(KIND=4),DIMENSION(2) ::Postn
	Postn=(/10,30/)
	R=4
	Mass=1.0
 call mysphere%Crt_Properties(R,Postn,Mass)
 call Gen_PROPERTIES(mysphere)
 call mysphere%Get_Properties()
END PROGRAM procdeclfn14

