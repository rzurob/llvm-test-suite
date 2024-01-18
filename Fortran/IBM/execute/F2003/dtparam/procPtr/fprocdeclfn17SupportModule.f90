!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : F2003/dtparam/procPtr/
! PROGRAMMER                 : Morteza Ershad-Manesh
! DATE                       : 07/29/2008
! PRIMARY FUNCTIONS TESTED   : procedure declaration statement & procedure component 
! DRIVER STANZA              : xlfF2003
! DESCRIPTION                : Use of procedure declaration statement
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


MODULE TypeDef

    TYPE, ABSTRACT :: Element(IKind,CLen)
	   INTEGER, KIND :: IKind
	   INTEGER, LEN  :: CLen
	END TYPE Element
    
	Type,EXTENDS(Element) :: ArrElement
	  INTEGER(KIND=IKind) :: Num
	  CHARACTER(LEN=CLen) :: ElmName
	END TYPE ArrElement

     TYPE :: Node
 	   TYPE(Node), pointer    :: Next,Prev
	   TYPE(ArrElement(4,20)) :: Elmnt
	   INTEGER(KIND=4)        :: ID=0
	 END TYPE
END MODULE

MODULE FunInterface

ABSTRACT INTERFACE
! This function creates a Unique ID for each 'node'
 INTEGER FUNCTION IID_LIST(NUM)
  use TypeDef
   INTEGER(KIND=4),INTENT(IN)    :: NUM                       !Number of ELEMENTS we need
 END FUNCTION

! This function creates our List
 INTEGER FUNCTION ICREATE_LIST(LastItem,CurrentItem,NUM)
  use TypeDef
   TYPE(Node), POINTER,INTENT(INOUT) :: LastItem,CurrentItem
   INTEGER(KIND=4),INTENT(IN)    :: NUM                       !Number of ELEMENTS we need
 END FUNCTION
 
 ! This function prints our List
 INTEGER FUNCTION IPRINT_LIST(LastItem,CurrentItem,NUM,REV)
  use TypeDef
   TYPE(Node), POINTER ,INTENT(IN) :: LastItem,CurrentItem
   INTEGER(KIND=4),INTENT(IN)    :: NUM                       !Number of ELEMENTS we need
   LOGICAL,INTENT(IN),OPTIONAL:: REV
 END FUNCTION   
 
! This function assigns the initial value to 2 input array.
INTEGER FUNCTION IINIT_List(LastItem,CurrentItem,NUM)
   use TypeDef
     TYPE(Node), POINTER ,INTENT(INOUT) :: LastItem,CurrentItem
     INTEGER(KIND=4),INTENT(IN)    :: NUM                       !Number of ELEMENTS we need
 END FUNCTION
 
! Perform CSHIFT on the TWO ARRAY
 INTEGER FUNCTION ICSHIFT_ARRAYS(LastItem,CurrentItem,NUM)
   use TypeDef
      TYPE(Node),POINTER,INTENT(INOUT)      :: LastItem,CurrentItem
	  INTEGER(KIND=4),INTENT(IN)            :: NUM
 END FUNCTION

! COMBINE TWO ARRAY AND RETURN THE COMBINED ARRAY
 FUNCTION ICOM_ARRAYS(Array1,Array2,Array3)
   use TypeDef
      TYPE(ArrElement(4,20)), INTENT(IN) :: Array1(20),Array2(20)
	  TYPE(ArrElement(4,20)), INTENT(OUT):: Array3(20,20)
 END FUNCTION

! DOT PRODUCT of two ARRAYS
 INTEGER FUNCTION IDPROC_ARRAYS_Int(Array1,Array2,Res)
   use TypeDef
      TYPE(ArrElement(4,20)), INTENT(IN) :: Array1(20),Array2(20)
	  INTEGER(KIND=4) , INTENT(OUT) :: Res
 END FUNCTION

! DOT PRODUCT of two ARRAYS
 REAL FUNCTION IDPROC_ARRAYS_Real(Array1,Array2,Res)
   use TypeDef
      TYPE(ArrElement(4,20)), INTENT(IN) :: Array1(20),Array2(20)
	  REAL(KIND=4) , INTENT(OUT) :: Res
 END FUNCTION

 
END INTERFACE
END MODULE



MODULE FunImplemention
  
CONTAINS 

! This function creates our List
 INTEGER FUNCTION CREATE_LIST(LastItem,CurrentItem,NUM)
  use TypeDef
   TYPE(Node), POINTER ,INTENT(INOUT) :: LastItem,CurrentItem
   INTEGER(KIND=4),INTENT(IN)    :: NUM                       !Number of ELEMENTS we need
     
       ALLOCATE(LastItem)
	   LastItem%Elmnt%Num = 0
       LastItem%Next => null() !LastItem
       LastItem%Prev =>LastItem
       CurrentItem=> LastItem

	 DO I=1,NUM-1   
      ALLOCATE(CurrentItem%Prev)
      CurrentItem%Prev%Next=>CurrentItem
      CurrentItem=>CurrentItem%Prev
	  CurrentItem%Elmnt%Num=I*2
     end do
	 CurrentItem%Prev=>null()
   CREATE_LIST=0
   
 END FUNCTION

 ! This function prints our List
 INTEGER FUNCTION PRINT_LIST(LastItem,CurrentItem,NUM,REV)
  use TypeDef
   TYPE(Node), POINTER ,INTENT(IN) :: LastItem,CurrentItem
   TYPE(Node), POINTER  :: tempLastItem,tempCurrentItem
   TYPE(Node)                        :: tset
   INTEGER(KIND=4),INTENT(IN)    :: NUM                       !Number of ELEMENTS we need
   LOGICAL,INTENT(IN),OPTIONAL   :: REV

   tempCurrentItem=>CurrentItem
   tempLastItem => LastItem

    DO I=1,NUM
     print*,tempCurrentItem%Elmnt%Num,"[",tempCurrentItem%ID,"]"
	 tempCurrentItem=>tempCurrentItem%Next
    END DO

   PRINT_LIST=0 
 END FUNCTION
 
! This function assigns the initial value to 2 input array.
 INTEGER FUNCTION INIT_List(LastItem,CurrentItem,NUM)
   use TypeDef
   use FunInterface
     TYPE(Node), POINTER ,INTENT(INOUT) :: LastItem,CurrentItem
     INTEGER(KIND=4),INTENT(IN)    :: NUM                       !Number of ELEMENTS we need
	 PROCEDURE(IID_LIST),POINTER,SAVE   :: ptr =>NULL()
	 ptr=>ID_LIST
   DO I=1,NUM
	LastItem%Elmnt%Num=I
	LastItem%ID=ptr(I)
	LastItem=>LastItem%Prev
   END DO
   INIT_List=0
 END FUNCTION
 
! This function creates a Unique ID for each 'node'
 INTEGER FUNCTION ID_LIST(NUM)
  use TypeDef
   INTEGER(KIND=4),INTENT(IN)    :: NUM                       !Number of ELEMENTS we need
   INTEGER(KIND=4)               :: temp                       !Number of ELEMENTS we need
   PROCEDURE(INTEGER),POINTER:: ptr => NULL()
   ID_LIST=(NUM*2008)*0805
 END FUNCTION

 INTEGER FUNCTION CSHIFT_ARRAYS(LastItem,CurrentItem,NUM)
   use TypeDef
      TYPE(Node),POINTER,INTENT(INOUT)      :: LastItem,CurrentItem
	  INTEGER(KIND=4),INTENT(IN)            :: NUM
	  TYPE(ArrElement(4,20))                :: tempArray(20),Array2(20)
	  
	  DO I=1,NUM
	   print*,CurrentItem%Elmnt%Num 
   	   CurrentItem=>CurrentItem%Next
      END DO

   CSHIFT_ARRAYS = 1
 END FUNCTION
 
END MODULE 