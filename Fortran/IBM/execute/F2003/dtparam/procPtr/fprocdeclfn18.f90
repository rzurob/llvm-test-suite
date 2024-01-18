!=======================================================================
! TEST BUCKET                : F2003/dtparam/procPtr/
! DATE                       : 08/13/2008
! PRIMARY FUNCTIONS TESTED   : procedure declaration statement & procedure component
! DESCRIPTION                : use of  procedure declaration statement in Functions and Main program and Select type
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012
MODULE M
  TYPE Base(k1)
   INTEGER, KIND :: k1
   INTEGER(K1)   :: I
   INTEGER(K1),POINTER   :: IntNum
   REAL(K1),POINTER      :: RealNum
   COMPLEX(K1),POINTER   :: CompNum
   LOGICAL(K1),POINTER   :: LogNum
  END TYPE

  TYPE,EXTENDS(Base)::Child1
    INTEGER(K1)   :: Num
  END TYPE

  TYPE,EXTENDS(Base)::Child2
    REAL(K1)   :: Num
  END TYPE

  TYPE,EXTENDS(Base)::Child3
    COMPLEX(K1)   :: Num
  END TYPE

  TYPE,EXTENDS(Base)::Child4
    LOGICAL(K1)   :: Num
  END TYPE

  TYPE,EXTENDS(BASE)::  Child5
    TYPE(child1(K1)),POINTER :: Inum
	TYPE(child2(K1)),POINTER :: Rnum
	TYPE(child3(K1)),POINTER :: Cnum
	TYPE(child4(K1)),POINTER :: Lnum
  END TYPE
END MODULE

MODULE N
 USE M

 INTERFACE SWITCH
  SUBROUTINE SADD_Child1_TYPES (C1,C2)
  import Child1
   TYPE(Child1(4)),  INTENT (INOUT) :: C1,C2
  END SUBROUTINE SADD_Child1_TYPES

  SUBROUTINE SADD_Child2_TYPES (C1,C2)
   import Child2
   TYPE(Child2(4)),  INTENT (INOUT) :: C1,C2
  END SUBROUTINE SADD_Child2_TYPES

  SUBROUTINE SADD_Child3_TYPES (C1,C2)
  import Child3
   TYPE(Child3(4)),  INTENT (INOUT) :: C1,C2
  END SUBROUTINE SADD_Child3_TYPES

  SUBROUTINE SADD_Child4_TYPES (C1,C2)
  import Child4
   TYPE(Child4(4)),  INTENT (INOUT) :: C1,C2
  END SUBROUTINE SADD_Child4_TYPES
 END INTERFACE SWITCH

END MODULE

  SUBROUTINE SADD_Child1_TYPES (x,y)
   USE M
   TYPE(Child1(4)), INTENT(INOUT) :: x,y
   y%Num=x%Num + y%Num
  END SUBROUTINE SADD_Child1_TYPES

  SUBROUTINE SADD_Child2_TYPES (x,y)
  USE M
   TYPE(Child2(4)), INTENT(INOUT) :: x,y
   y%Num=x%Num + y%Num
  END SUBROUTINE SADD_Child2_TYPES

  SUBROUTINE SADD_Child3_TYPES (x,y)
  USE M
   TYPE(Child3(4)), INTENT(INOUT) :: x,y
   y%Num=x%Num + y%Num
  END SUBROUTINE SADD_Child3_TYPES

  SUBROUTINE SADD_Child4_TYPES (x,y)
  USE M
   TYPE(Child4(4)), INTENT(INOUT) :: x,y
   y%Num=x%Num .AND. y%Num
  END SUBROUTINE SADD_Child4_TYPES

MODULE FnInt
USE N
 INTERFACE
  SUBROUTINE IWhat_TYPES (x,y)
  import Base
   CLASS(Base(4)), INTENT(INOUT) :: x,y
  END SUBROUTINE IWhat_TYPES

  SUBROUTINE IADD_Child_TYPES (x,y)
  import Child1
  import Child5
   TYPE(Child1(4)), INTENT(INOUT) :: x
   TYPE(Child5(4)), INTENT(INOUT) :: y
  END SUBROUTINE IADD_Child_TYPES

  SUBROUTINE IADD_Child1_TYPES (x,y)
  import Child1
   TYPE(Child1(4)), INTENT(INOUT) :: x,y
  END SUBROUTINE IADD_Child1_TYPES

  SUBROUTINE IADD_Child2_TYPES (x,y)
  import Child2
   TYPE(Child2(4)), INTENT(INOUT) :: x,y
  END SUBROUTINE IADD_Child2_TYPES

  SUBROUTINE IADD_Child3_TYPES (x,y)
  import Child3
   TYPE(Child3(4)), INTENT(INOUT) :: x,y
  END SUBROUTINE IADD_Child3_TYPES

  SUBROUTINE IADD_Child4_TYPES (x,y)
  import Child4
   TYPE(Child4(4)), INTENT(INOUT) :: x,y
  END SUBROUTINE IADD_Child4_TYPES

  SUBROUTINE IADD_Child5_TYPES (x,y)
  import Child5
   TYPE(Child5(4)), INTENT(INOUT) :: x,y
  END SUBROUTINE IADD_Child5_TYPES

  SUBROUTINE ITransferElmn (x,y)
  import Base
   CLASS(Base(4)), INTENT(INOUT) :: x,y
  END SUBROUTINE ITransferElmn
 END INTERFACE

 CONTAINS
  SUBROUTINE What_TYPES (xin,yin)
   CLASS(Base(4)), INTENT(INOUT) :: xin,yin
   PROCEDURE(IADD_Child_TYPES), POINTER :: procptr0=>null()
   PROCEDURE(IADD_Child1_TYPES), POINTER :: procptr1=>null()
   PROCEDURE(IADD_Child2_TYPES), POINTER :: procptr2=>null()
   PROCEDURE(IADD_Child3_TYPES), POINTER :: procptr3=>null()
   PROCEDURE(IADD_Child4_TYPES), POINTER :: procptr4=>null()
   PROCEDURE(IADD_Child5_TYPES), POINTER :: procptr5=>null()

    SELECT TYPE (yin)
	 TYPE IS (Child1(4))
	  PRINT*,"y - TYPE is Child1(4)"
       SELECT TYPE (xin)
    	 TYPE IS (Child1(4))
	      PRINT*,"x - TYPE is Child1(4)"
	      procptr1=>ADD_Child1_TYPES
		  call procptr1(xin,yin)
		 TYPE IS (Child5(4))
		  PRINT*,"x - TYPE is Child5(4)"
		  procptr0=>ADD_Child1a_TYPES
		  call procptr0(yin,xin)
         CLASS DEFAULT
		  print*,"Unknown Type"
       END SELECT
	 TYPE IS (Child2(4))
	  PRINT*,"y - TYPE is Child2(4)"
	   SELECT TYPE (xin)
    	 TYPE IS (Child2(4))
	      PRINT*,"x - TYPE is Child2(4)"
	      procptr2=>ADD_Child2_TYPES
		  call procptr2(xin,yin)
         CLASS DEFAULT
		  print*,"Unknown Type"
       END SELECT
	 TYPE IS (Child3(4))
	  PRINT*,"y - TYPE is Child3(4)"
	   SELECT TYPE (xin)
    	 TYPE IS (Child3(4))
	      PRINT*,"x - TYPE is Child3(4)"
	      procptr3=>ADD_Child3_TYPES
		  call procptr3(xin,yin)
         CLASS DEFAULT
		  print*,"Unknown Type"
       END SELECT
	 TYPE IS (Child4(4))
	  PRINT*,"y - TYPE is Child4(4)"
	   SELECT TYPE (xin)
    	 TYPE IS (Child4(4))
	      PRINT*,"x - TYPE is Child4(4)"
	      procptr4=>ADD_Child4_TYPES
		  call procptr4(xin,yin)
         CLASS DEFAULT
		  print*,"Unknown Type"
       END SELECT
	 TYPE IS (Child5(4))
	  PRINT*,"y - TYPE is Child5(4)"
	   SELECT TYPE (xin)
    	 TYPE IS (Child5(4))
	      PRINT*,"x - TYPE is Child5(4)"
		  call procptr5(xin,yin)
         CLASS DEFAULT
		  print*,"Unknown Type"
       END SELECT
	END SELECT
  END SUBROUTINE What_TYPES

  SUBROUTINE ADD_Child1_TYPES (x,y)
   TYPE(Child1(4)), INTENT(INOUT) :: x,y
   y%Num=x%Num + y%Num
  END SUBROUTINE ADD_Child1_TYPES

  SUBROUTINE ADD_Child2_TYPES (x,y)
   TYPE(Child2(4)), INTENT(INOUT) :: x,y
   y%Num=x%Num + y%Num
  END SUBROUTINE ADD_Child2_TYPES

  SUBROUTINE ADD_Child3_TYPES (x,y)
   TYPE(Child3(4)), INTENT(INOUT) :: x,y
   y%Num=x%Num + y%Num
  END SUBROUTINE ADD_Child3_TYPES

  SUBROUTINE ADD_Child4_TYPES (x,y)
   TYPE(Child4(4)), INTENT(INOUT) :: x,y
   y%Num=x%Num .AND. y%Num
  END SUBROUTINE ADD_Child4_TYPES

    SUBROUTINE ADD_Child1a_TYPES (x,y)
   TYPE(Child1(4)), INTENT(INOUT) :: x
   TYPE(Child5(4)), INTENT(INOUT) :: y
   y%Inum%Num=x%Num + y%Inum%Num
  END SUBROUTINE ADD_Child1a_TYPES

  SUBROUTINE ADD_Child2a_TYPES (x,y)
   TYPE(Child2(4)), INTENT(INOUT) :: x
   TYPE(Child5(4)), INTENT(INOUT) :: y
   y%Rnum%Num=x%Num + y%Rnum%Num
  END SUBROUTINE ADD_Child2a_TYPES

  SUBROUTINE ADD_Child3a_TYPES (x,y)
   TYPE(Child3(4)), INTENT(INOUT) :: x
   TYPE(Child5(4)), INTENT(INOUT) :: y
   y%Cnum%Num=x%Num + y%Cnum%Num
  END SUBROUTINE ADD_Child3a_TYPES

  SUBROUTINE ADD_Child4a_TYPES (x,y)
   TYPE(Child4(4)), INTENT(INOUT) :: x
   TYPE(Child5(4)), INTENT(INOUT) :: y
   y%Lnum%Num=x%Num .AND. y%Lnum%Num
  END SUBROUTINE ADD_Child4a_TYPES

END MODULE

PROGRAM procdeclfn18
USE N
USE FnInt

 PROCEDURE(IWhat_TYPES), POINTER :: procptr=>null()

 TYPE(Base(4)):: B1
 TYPE(Child5(4))  :: T5

 TYPE(Child1(4)),TARGET:: T1a,T1b
 TYPE(Child2(4)),TARGET:: T2a,T2b
 TYPE(Child3(4)),TARGET:: T3a,T3b
 TYPE(Child4(4)),TARGET:: T4a,T4b

 TYPE(Child1(4)),POINTER:: T1c,T1d
 TYPE(Child2(4)),POINTER:: T2c,T2d
 TYPE(Child3(4)),POINTER:: T3c,T3d
 TYPE(Child4(4)),POINTER:: T4c,T4d

 procptr=>What_TYPES

 !INTEGER
 T1c=>T1a
 T1d=>T1b
 T1c%Num=20
 T1d%Num=80
 !REAL
 T2c=>T2a
 T2d=>T2b
 T2c%Num=15.5
 T2d%Num=95.3
 !COMPLEX
 T3c=>T3a
 T3d=>T3b
 T3c%Num=(15.5,-20)
 T3d%Num=(5.5,-5)
 !LOGICAL
 T4c=>T4a
 T4d=>T4b
 T4c%Num=.TRUE.
 T4d%Num=.FALSE.

 T5%Inum=>T1d
 T5%Rnum=>T2d
 T5%Cnum=>T3d
 T5%Lnum=>T4d

  print*,"Integer:"
 print*,"Using Procedure pointer:"
 call procptr(T1a,T1b)
 print*,T5%Inum%Num
 print*,"Using subroutine call:"
 call What_TYPES	(T1a,T1b)
 print*,T5%Inum%Num
 print*,"Using INTERFACE SWITCH:"
 call SWITCH(T1a,T1a)
 print*,T1a%Num
  print*,""
  print*,"Real:"
 print*,"Using Procedure pointer:"
 call procptr(T2a,T2b)
 print*,T5%Rnum%Num
 print*,"Using subroutine call:"
 call What_TYPES	(T2a,T2b)
 print*,T5%Rnum%Num
 print*,"Using INTERFACE SWITCH:"
 call SWITCH(T2a,T2a)
 print*,T2a%Num
  print*,""
  print*,"Complex:"
 print*,"Using Procedure pointer:"
 call procptr(T3a,T3b)
 print*,T5%Cnum%Num
 print*,"Using subroutine call:"
 call What_TYPES	(T3a,T3b)
 print*,T5%Cnum%Num
 print*,"Using INTERFACE SWITCH:"
 call SWITCH(T3a,T3a)
 print*,T3a%Num
  print*,""
  print*,"Logical:"
 print*,"Using Procedure pointer:"
 call procptr(T4a,T4b)
 print*,T5%Lnum%Num
 print*,"Using subroutine call:"
 call What_TYPES	(T4a,T4b)
 print*,T5%Lnum%Num
 print*,"Using INTERFACE SWITCH:"
 call SWITCH(T4a,T4a)
 print*,T4a%Num
  print*,""
  print*,"Using Type definition t5:"
 print*,"Using Procedure pointer:"
 call procptr(T5,T1b)
 print*,T5%Inum%Num
 print*,"Using subroutine call:"
 call What_TYPES   (T5,T1b)
 print*,T5%Inum%Num



 END PROGRAM procdeclfn18