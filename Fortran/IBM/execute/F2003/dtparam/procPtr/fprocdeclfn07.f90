!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : F2003/dtparam/procPtr/
! PROGRAMMER                 : Morteza Ershad-Manesh
! DATE                       : 08/05/2008
! PRIMARY FUNCTIONS TESTED   : procedure declaration statement & procedure component 
! DRIVER STANZA              : xlfF2003
! DESCRIPTION                : Use of  procedure declaration statement & procedure component with NO/PASS attr. 
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012
MODULE types
INTERFACE
  SUBROUTINE PrntChar4 ()
   INTEGER(KIND=4),SAVE    :: NumCalled =1
 END SUBROUTINE
   SUBROUTINE PrntChar4a ()
   	   INTEGER(KIND=4),SAVE    :: NumCalled =1
 END SUBROUTINE
END INTERFACE
  TYPE Base(k1,LEN1) 
   INTEGER, KIND   :: k1
   INTEGER, LEN    :: LEN1
   INTEGER(K1)     :: I
   CHARACTER(LEN=LEN1) :: OurChar
   PROCEDURE(PrntChar4), NOPASS,POINTER  :: procptr => null()
   PROCEDURE(PrntChar4a), NOPASS,POINTER :: procptr2 => null()
   CONTAINS
   PROCEDURE, PASS(Base1)  :: procptr1 => PrntChar6
  END TYPE 
  
  TYPE,EXTENDS(Base) :: Base2
   PROCEDURE(PrntChar4),NOPASS,POINTER  :: procptr4 => null()
   CONTAINS
   PROCEDURE, PASS(Base1a)  :: procptr3 => PrntChar7
  END TYPE
  
 CONTAINS
 SUBROUTINE PrntChar4b ()
   	   INTEGER(KIND=4),SAVE    :: NumCalled =1
	   print*,"In PrntChar4b, NumCalled ",NumCalled
 END SUBROUTINE
 
   SUBROUTINE PrntChar6 (Base1)
	CLASS(Base(4,*)),INTENT(IN) :: Base1
    INTEGER(KIND=4),SAVE    :: NumCalled =1
	
	SELECT TYPE (Base1)
	  TYPE IS (Base(4,*))
	   print*,"In PrntChar6, Got the following String from the passed object: ",Base1%OurChar
	   CLASS IS (Base(4,*))
	  CLASS DEFAULT
	   print*,"UNKNOWN TYPE"
	END SELECT
   END SUBROUTINE
 
    SUBROUTINE PrntChar7 (Base1a)
	CLASS(Base2(4,*)),INTENT(INOUT) :: Base1a
    INTEGER(KIND=4),SAVE    :: NumCalled =1
	
	SELECT TYPE (Base1a)
	  TYPE IS (Base2(4,*))
	   print*,"In PrntChar7, Got the following String from the passed object: ",Base1a%OurChar
	   CLASS IS (Base2(4,*))
	  CLASS DEFAULT
	   print*,"UNKNOWN TYPE"
	END SELECT
   END SUBROUTINE
 
END MODULE

MODULE m
USE types


 CONTAINS
  SUBROUTINE PrntChar5 ()
   INTEGER(KIND=4),SAVE    :: NumCalled =1
   print*,"PrntChar4 has been called,",NumCalled," times"
   NumCalled=NumCalled+1
 END SUBROUTINE
 
 SUBROUTINE AsstPtr(a)
  TYPE(Base(4,20)),INTENT(INOUT) :: a
   a%procptr=>PrntChar5
 END SUBROUTINE
 
 SUBROUTINE PrntChar1 (a)
   TYPE(Base(4,20)),INTENT(INOUT) :: a
   a%procptr2=>PrntChar5
   print*,"PrntValue 1 , the value of the character is ",a%OurChar
   print*,"Is procptr associated? ", associated(a%procptr)
   call a%procptr2()
 END SUBROUTINE
 
 SUBROUTINE PrntChar2 (b)
  TYPE(Base(4,20)),INTENT(INOUT) :: b
  INTEGER, SAVE :: N=2
  b%procptr2=>PrntChar5
  print*,"PrntValue",N,", the value of the character is ",b%OurChar
  print*,"Is procptr associated? ", associated(b%procptr)
  call b%procptr2()
   N=N+1
 END SUBROUTINE
 
 SUBROUTINE PrntChar3 (b)
  TYPE(Base(4,20)),INTENT(INOUT) :: b
  INTEGER, SAVE :: N=2
    b%procptr2=>PrntChar5
  print*,"PrntValue 3 , the value of the character is ",b%OurChar, " and when it is adjusted left ", ADJUSTL(b%OurChar),"."
  print*,"Is procptr associated? ", associated(b%procptr)
  call b%procptr2()
  N=N+1
 END SUBROUTINE 

 SUBROUTINE callprntValue2(b)
  TYPE(Base(4,20)),INTENT(INOUT) :: b
  PROCEDURE(PrntChar2),POINTER :: ptr   ! RETURN TYPE OF OUR FN IS A POINTER 
  ptr=>PrntChar2
  b%i=20
  call ptr(b)
 END SUBROUTINE
 
 FUNCTION ChangePtr(i)
   INTEGER,INTENT(IN) :: i
   PROCEDURE(PrntChar1),POINTER :: ChangePtr   ! RETURN TYPE OF OUR FN IS A POINTER 
     
   IF ( i .EQ. 1) THEN
    ChangePtr=>PrntChar1
   ELSE IF (i .EQ. 2) THEN
    ChangePtr=>PrntChar2
   ELSE
    ChangePtr=>PrntChar2
   ENDIF
  END FUNCTION
  
   FUNCTION ChangePtr2(i,c)
   INTEGER,INTENT(IN) :: i
   CLASS(Base(4,*)),INTENT(IN):: c
   PROCEDURE(PrntChar1),POINTER :: ChangePtr2   ! RETURN TYPE OF OUR FN IS A POINTER 
   

   SELECT TYPE (c)
    TYPE IS (Base(4,*))
	 print*,"TYPE IS BASE(4,",c%LEN1,")"
    CLASS IS (Base(4,*))
	 print*,"CLASS IS BASE(4,",c%LEN1,")"
   END SELECT
   
   	  ChangePtr2=>PrntChar3
   
  END FUNCTION
END MODULE

PROGRAM procdeclfn07
USE M

 PROCEDURE(PrntChar2),POINTER :: procptr1=> null()
 PROCEDURE(),POINTER :: procptr2=> null()
 TYPE(Base(4,20)),TARGET :: n
 TYPE(Base2(4,20)),TARGET :: n2
 CLASS(Base(4,20)),POINTER :: x
 
  n=Base(4,20)(15, "ABCDEFGHIJ")
  n2%OurChar=n%OurChar
  n2%I=n%LEN1
  
 print*,"Before association"
 procptr1 => ChangePtr(1)
 call procptr1(n)
 
 procptr1 => ChangePtr(3)
 call procptr1(n)

 procptr1 => ChangePtr2(2,Base(4,20)(15,"ABCDEFGHIJ"))
 call procptr1(n)

 print*,"After association"
 procptr1 => AsstPtr
 call procptr1(n)
 procptr1 => ChangePtr(1)
 call procptr1(n)
 procptr1 => ChangePtr(3)
 call procptr1(n)
 procptr1 => ChangePtr2(2,Base(4,20)(15,"ABCDEFGHIJ"))
 call procptr1(n)
 call n%procptr1()
 print*,"using n2, Type Base2"
 call n2%procptr3()
 n2%procptr2=>PrntChar4b
 call n2%procptr2
 
END PROGRAM procdeclfn07

