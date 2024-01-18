!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : F2003/dtparam/procPtr/
! PROGRAMMER                 : Morteza Ershad-Manesh
! DATE                       : 08/13/2008
! PRIMARY FUNCTIONS TESTED   : procedure declaration statement & procedure component 
! DRIVER STANZA              : xlfF2003
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
   INTEGER(K1)   :: I = 0
   INTEGER(K1)   :: IntNum
   REAL(K1)      :: RealNum
   COMPLEX(K1)   :: CompNum
   LOGICAL(K1)   :: LogNum = .false.
  END TYPE 

  INTERFACE
   SUBROUTINE Iphase(x)
   import Base
    TYPE(Base(4)),ALLOCATABLE,INTENT(INOUT) :: x
   END SUBROUTINE
   SUBROUTINE Idephase(x,n)
   import Base
    TYPE(Base(4)),ALLOCATABLE,INTENT(INOUT) :: x
    INTEGER(KIND=4),INTENT(IN) ::n
   END SUBROUTINE
  END INTERFACE
  
  CONTAINS
   SUBROUTINE phase1(x)
    TYPE(Base(4)),ALLOCATABLE,INTENT(INOUT) :: x
    call phase2(x)
   END SUBROUTINE

   SUBROUTINE phase2(x)
    TYPE(Base(4)),ALLOCATABLE,INTENT(INOUT) :: x
    call phase3(x)
   END SUBROUTINE

   SUBROUTINE phase3(x)
    TYPE(Base(4)),ALLOCATABLE,INTENT(INOUT) :: x
    ALLOCATE(x)
   END SUBROUTINE

   SUBROUTINE Dephase(x)
    TYPE(Base(4)),ALLOCATABLE,INTENT(INOUT) :: x
    DEALLOCATE(x)
   END SUBROUTINE

   
   SUBROUTINE Copy(x,n)
    TYPE(Base(4)),ALLOCATABLE,INTENT(INOUT) :: x
    INTEGER(KIND=4),INTENT(IN) :: n
    TYPE(Base(4)),SAVE :: TEMP
     IF ( n .EQ. 0 ) THEN
      TEMP=x
     ELSE
      x=TEMP
     END IF
   END SUBROUTINE

   SUBROUTINE Assignphase4(x)
    TYPE(Base(4)),ALLOCATABLE,INTENT(INOUT) :: x
    PROCEDURE(Iphase),pointer :: procptr2=> NULL()
    
    x%LogNum = .NOT. x%LogNum
    
   END SUBROUTINE 
   
   SUBROUTINE Assignphase3(x)
    TYPE(Base(4)),ALLOCATABLE,INTENT(INOUT) :: x
    PROCEDURE(Iphase),pointer :: procptr2=> NULL()
    
    x%CompNum = (2,30)*(2,-10)
    procptr2=>Assignphase4
    call procptr2(x)
   END SUBROUTINE 
   
   SUBROUTINE Assignphase2(x)
    TYPE(Base(4)),ALLOCATABLE,INTENT(INOUT) :: x
    PROCEDURE(Iphase),pointer :: procptr2=> NULL()
    procptr2=>Assignphase3
    x%RealNum =(30.5)**3
    call Assignphase3(x)
   END SUBROUTINE 
   
   SUBROUTINE Assignphase1(x)
    TYPE(Base(4)),ALLOCATABLE,INTENT(INOUT) :: x
    PROCEDURE(Iphase),pointer :: procptr1=>NULL()
    INTEGER(Kind=4) :: N
    procptr1=>Assignphase2
    N=x%IntNum
    x%IntNum=20
    call procptr1(x)
    
   END SUBROUTINE
END MODULE M


PROGRAM procdeclfn20
USE M
 TYPE(Base(4)), ALLOCATABLE :: myBase0,myBase1
 PROCEDURE(Iphase),POINTER :: procptr => NULL()
 PROCEDURE(Idephase),POINTER :: procptr1 => NULL()
 
 print*,"TEST PART 1"
 procptr=>phase1
 print*,"ALLOCATING"
 call procptr(myBase0)
 call phase3(myBase1)
 print*,"myBase0 is allocated:",Allocated(myBase0)
 print*,"myBase1 is allocated:",Allocated(myBase1) 
 
 myBase0%IntNum=10
 myBase0%RealNum=33.33
 myBase0%CompNum=(30,-4)
 myBase0%LogNum=.True.
 
 print*,"COPYING RESULT FROM myBase0 to myBase1"
 procptr1=>Copy
 call procptr1(myBase0,0)
 call procptr1(myBase1,1)
 IF (myBase0%IntNum .ne. myBase1%IntNum .OR.   &
     myBase0%RealNum .ne. myBase1%RealNum .OR. &
     myBase0%CompNum .ne. myBase1%CompNum .OR. &
     myBase0%LogNum .neqv. myBase1%LogNum ) THEN
  STOP 1
 END IF
 print*,myBase0
 print*,myBase1
 print*,"DEALLOCATING"
 procptr=>Dephase
 call procptr(myBase0)
 call procptr(myBase1)
 print*,"myBase0 is allocated:",Allocated(myBase0)
 print*,"myBase1 is allocated:",Allocated(myBase1)
 print*,""
 print*,"TEST PART 2"
 print*,"ALLOCATING"
 procptr=>phase1
 call procptr(myBase0)
 call phase3(myBase1)
 print*,"myBase0 is deallocated:",Allocated(myBase0)
 print*,"myBase1 is deallocated:",Allocated(myBase1) 
 
 procptr=>Assignphase1
 call procptr(myBase0)
 
 print*,"COPYING RESULT FROM myBase0 to myBase1"
 procptr1=>Copy
 call procptr1(myBase0,0)
 call procptr1(myBase1,1)
 IF (myBase0%IntNum .ne. myBase1%IntNum .OR.   &
     myBase0%RealNum .ne. myBase1%RealNum .OR. &
     myBase0%CompNum .ne. myBase1%CompNum .OR. &
     myBase0%LogNum .neqv. myBase1%LogNum ) THEN
  STOP 1
 END IF
 print*,myBase0
 print*,myBase1
 print*,"DEALLOCATING"
 procptr=>Dephase
 call procptr(myBase0)
 call procptr(myBase1)
 print*,"myBase0 is deallocated:",Allocated(myBase0)
 print*,"myBase1 is deallocated:",Allocated(myBase1)
 print*,""
END PROGRAM procdeclfn20
