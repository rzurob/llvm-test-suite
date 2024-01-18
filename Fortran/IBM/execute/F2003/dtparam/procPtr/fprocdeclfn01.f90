!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : F2003/dtparam/procPtr/
! PROGRAMMER                 : Morteza Ershad-Manesh
! DATE                       : 08/05/2008
! PRIMARY FUNCTIONS TESTED   : procedure declaration statement & procedure component 
! DRIVER STANZA              : xlfF2003
! DESCRIPTION                : use of  procedure declaration statement in Functions and Main program
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012
MODULE m
  TYPE Base(k1) 
   INTEGER, KIND :: k1
   INTEGER(K1)   :: I
  END TYPE 
 CONTAINS
 SUBROUTINE PrntValue1 (a)
  TYPE(Base(4)),INTENT(IN) :: a
  print*,"PrntValue 1 , the value of the input is ",a
 END SUBROUTINE
 
 SUBROUTINE PrntValue2 (b)
  TYPE(Base(4)),INTENT(IN) :: b
  INTEGER, SAVE :: N=2
  print*,"PrntValue",N,", the value of the input is ",b
  N=N+1
 END SUBROUTINE
 
 

 SUBROUTINE callprntValue2(b)
  TYPE(Base(4)),INTENT(INOUT) :: b
  PROCEDURE(PrntValue2),POINTER :: ptr   ! RETURN TYPE OF OUR FN IS A POINTER 
  ptr=>PrntValue2
  b%i=20
  call ptr(b)
 END SUBROUTINE
 
 FUNCTION ChangePtr(i,c)
   INTEGER,INTENT(IN) :: i
   TYPE(Base(4)),INTENT(IN), OPTIONAL :: c
   PROCEDURE(PrntValue1),POINTER :: ChangePtr   ! RETURN TYPE OF OUR FN IS A POINTER 
  
   IF ( i .EQ. 1) THEN
    ChangePtr=>PrntValue1
   ELSE IF (i .EQ. 2) THEN
    ChangePtr=>callprntValue2
   ELSE
    ChangePtr=>PrntValue2
   ENDIF
  END FUNCTION
END MODULE

PROGRAM procdeclfn01
USE M
 PROCEDURE(PrntValue2),POINTER :: procptr1=> null()
  
 
 procptr1 => ChangePtr(1)
 call procptr1(Base(4)(5))
 
 procptr1 => ChangePtr(3)
 call procptr1(Base(4)(15))

 procptr1 => ChangePtr(2)
 call procptr1(Base(4)(15))
 
END PROGRAM procdeclfn01