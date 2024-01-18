!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : F2003/dtparam/procPtr/
! PROGRAMMER                 : Morteza Ershad-Manesh
! DATE                       : 07/29/2008
! PRIMARY FUNCTIONS TESTED   : procedure declaration statement & procedure component 
! DRIVER STANZA              : xlfF2003
! DESCRIPTION                : Use of DEFERRED procedure declaration statement and procedure component statment
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012
MODULE OurTypeDef

    TYPE, ABSTRACT :: OBJECT(CLen,RKind)
	  INTEGER, KIND      :: RKind
	  INTEGER, LEN       :: CLen
	  REAL(KIND=RKind), DIMENSION(3) :: POSITION=(/0.0,0.0,0.0/)
	  CHARACTER(len=CLen)      :: NAME='Object'
	  LOGICAL            :: HIDDEN=.false.
	END TYPE OBJECT

	TYPE,EXTENDS(OBJECT),ABSTRACT :: DRAWABLE_OBJ 
	 INTEGER(KIND=RKind), DIMENSION(3)   :: COLOR=(/0.0,0.0,0.0/)
	 CONTAINS
      PROCEDURE(PREPARE_OBJ),PASS(OBJ),DEFERRED :: PrepObj
	END TYPE DRAWABLE_OBJ

	ABSTRACT INTERFACE
	 SUBROUTINE PREPARE_OBJ(OBJ)
	 import DRAWABLE_OBJ
	  CLASS(DRAWABLE_OBJ(*,4)),INTENT(IN) :: OBJ
	 END SUBROUTINE PREPARE_OBJ
 	END INTERFACE
END MODULE

MODULE OBJDefinitions
 USE OurTypeDef
 
  TYPE,EXTENDS(DRAWABLE_OBJ):: TRIANGLE
   REAL(KIND=RKind),DIMENSION(3,3) :: VERTICES
    CONTAINS
     PROCEDURE,PASS(OBJ) :: PrepObj=>PREPARE_TRIANGLE
  END TYPE

 CONTAINS
  SUBROUTINE PREPARE_TRIANGLE (OBJ)
   CLASS(TRIANGLE(*,4)),INTENT(IN) ::OBJ
    print*,"TRIANGLE NAME:",OBJ%NAME
    print*,"TRIANGLE COLOR:",OBJ%COLOR
    print*,"TRIANGLE VERTICES:"
    print*," A:",OBJ%VERTICES(1,:)
    print*," B:",OBJ%VERTICES(2,:)
    print*," C:",OBJ%VERTICES(3,:)
    print*,""
  END SUBROUTINE PREPARE_TRIANGLE 
   
 SUBROUTINE TRIANGLE_Z_OVERLAP(OBJ1,OBJ2)
   TYPE(TRIANGLE(20,4)),INTENT(IN) :: OBJ1,OBJ2
   IF ((OBJ1%VERTICES(1,3) .LT. OBJ2%VERTICES(1,3)) .AND. ( (OBJ1%VERTICES(2,3) .LT. OBJ2%VERTICES(2,3))  .OR.  (OBJ1%VERTICES(3,3) .LT. OBJ2%VERTICES(3,3)) )) THEN
        PRINT*,OBJ1%NAME," Z axis, is CLOSER to the camera then the ",OBJ2%NAME
	ELSE IF ((OBJ1%VERTICES(1,3) .EQ. OBJ2%VERTICES(1,3)) .AND. ( (OBJ1%VERTICES(2,3) .EQ. OBJ2%VERTICES(2,3))  .OR.  (OBJ1%VERTICES(3,3) .EQ. OBJ2%VERTICES(3,3)) )) THEN
	    PRINT*,OBJ1%NAME," and ",OBJ2%NAME, " Z axis are equal "
   ENDIF
  END SUBROUTINE TRIANGLE_Z_OVERLAP

  FUNCTION ChangePtr ()
     INTEGER :: ChangePtr
  ! TYPE(InterfaceChangePtr) ChangePtr
  PROCEDURE(IChangePtr2),POINTER :: Ptr2
  INTEGER             :: Rtncode

   ABSTRACT INTERFACE
   FUNCTION IChangePtr2 ()
    END FUNCTION
   END INTERFACE  

   print*,"END OF EXECUTION OF PROGRAM procdeclfn16"

   ChangePtr = 0  !Ptr2()
  END FUNCTION
END MODULE

PROGRAM procdeclfn16
 USE OBJDefinitions
 TYPE(TRIANGLE(20,4)),TARGET  :: Red_Triangle,Blue_Triangle,GREEN_Triangle
 TYPE(TRIANGLE(20,4)),POINTER :: Triangle1,Triangle2
 INTEGER                      :: Rtncode
 PROCEDURE(Z_OVERLAP),POINTER :: procptr => null()
 PROCEDURE(InterfaceChangePtr),POINTER :: procptr2 => null()
 
 ABSTRACT INTERFACE
  SUBROUTINE Z_OVERLAP(OBJ1,OBJ2)
   use OBJDefinitions
   TYPE(TRIANGLE(20,4)),INTENT(IN) :: OBJ1,OBJ2
  END SUBROUTINE

  FUNCTION InterfaceChangePtr ()
  END FUNCTION
END INTERFACE

 Red_Triangle%NAME='RED Triangle'
 Red_Triangle%COLOR=(/255,0,0/)
 Red_Triangle%VERTICES(1,:)=(/10,10,10/)
 Red_Triangle%VERTICES(2,:)=(/15,12,10/)
 Red_Triangle%VERTICES(3,:)=(/5,12,10/)

 call Red_Triangle%PrepObj
 Triangle1=>Red_Triangle

 Blue_Triangle%NAME='BLUE Triangle'
 Blue_Triangle%COLOR=(/0,0,255/)
 Blue_Triangle%VERTICES(1,:)=(/5,5,0/)
 Blue_Triangle%VERTICES(2,:)=(/10,12,0/)
 Blue_Triangle%VERTICES(3,:)=(/0,12,0/)
 
 call Blue_Triangle%PrepObj
 Triangle2=>Blue_Triangle
 
 procptr=>TRIANGLE_Z_OVERLAP
 PRINT*,"CHECK TO SEE IF Z AXIS OF ",Triangle2%NAME," is closer then ",Triangle1%NAME
 call procptr(Triangle2,Triangle1)
 PRINT*,""  
  
 GREEN_Triangle%NAME='GREEN Triangle'
 GREEN_Triangle%COLOR=(/0,0,255/)
 GREEN_Triangle%VERTICES(1,:)=Triangle2%VERTICES(1,:)
 GREEN_Triangle%VERTICES(2,:)=Triangle2%VERTICES(2,:)
 GREEN_Triangle%VERTICES(3,:)=Triangle2%VERTICES(3,:)
 
 call GREEN_Triangle%PrepObj
 Triangle1=>GREEN_Triangle
 
 PRINT*,"CHECK TO SEE IF Z AXIS OF ",Triangle2%NAME," is closer then ",Triangle1%NAME
   call procptr(Triangle2,Triangle1)
 print*,""  
 
 procptr2=>ChangePtr
 Rtncode=ChangePtr()
 call procptr2()           ! This line should display Nothing

END PROGRAM  procdeclfn16
