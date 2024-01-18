!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : F2003/dtparam/procPtr/
! PROGRAMMER                 : Morteza Ershad-Manesh
! DATE                       : 08/05/2008
! PRIMARY FUNCTIONS TESTED   : procedure declaration statement & procedure component 
! DRIVER STANZA              : xlfF2003
! DESCRIPTION                : Use of procedure declaration statement in functions and subroutines
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012
MODULE types
  TYPE Base(k1,LEN1) 
   INTEGER, KIND   :: k1
   INTEGER, LEN    :: LEN1
   INTEGER(K1)     :: I
   CHARACTER(LEN=LEN1) :: OurChar
  END TYPE 
END MODULE

MODULE m
USE types

 CONTAINS
 SUBROUTINE PrntChar1 (a)
   TYPE(Base(4,20)),INTENT(IN) :: a
   print*,"PrntValue 1 , the value of the character is ",a%OurChar
 END SUBROUTINE
 
 SUBROUTINE PrntChar2 (b)
  TYPE(Base(4,20)),INTENT(IN) :: b
  INTEGER, SAVE :: N=2
   print*,"PrntValue",N,", the value of the character is ",b%OurChar
  N=N+1
 END SUBROUTINE
 
 SUBROUTINE PrntChar3 (b)
  TYPE(Base(4,20)),INTENT(IN) :: b
  INTEGER, SAVE :: N=2

  print*,"PrntValue 3 , the value of the character is ",b%OurChar, " and when it is adjusted left ", ADJUSTL(b%OurChar),"."
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

PROGRAM procdeclfn02
USE M
 PROCEDURE(PrntChar2),POINTER :: procptr1=> null()
  
 
 procptr1 => ChangePtr(1)
 call procptr1(Base(4,20)(5,"ABCDEFGHIJ"))
 
 procptr1 => ChangePtr(3)
 call procptr1(Base(4,20)(15,"ABCDEFGHIJ"))

 procptr1 => ChangePtr2(2,Base(4,20)(15,"ABCDEFGHIJ"))
 call procptr1(Base(4,20)(15,"ABCDEFGHIJ"))
 
END PROGRAM procdeclfn02

