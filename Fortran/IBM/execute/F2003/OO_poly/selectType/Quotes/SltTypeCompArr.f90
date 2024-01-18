! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: SltTypeCompArr.f
! %VERIFY:  
! %STDIN:
! %STDOUT: 
! %EXECARGS:
! %POSTCMD: 
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : SltTypeCompArr
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Dec. 14, 2004
!*  ORIGIN                     : AIX Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type 
!*
!*  SECONDARY FUNCTIONS TESTED : Selector 
!*
!*  REFERENCE                  : Feature 219934.OO_poly
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*     
!*   The type spec is specified with a type with variuos Array components 
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE :: Base
      INTEGER :: BaseId=-1
      TYPE(Base), POINTER :: BaseArr(:)=>NULL()
    END TYPE

    TYPE, ABSTRACT,  EXTENDS(Base) :: Base1
      INTEGER :: Base1Id=1
      TYPE(BASE), POINTER :: BaseArr1(:)
    END TYPE

    TYPE, EXTENDS(Base1) :: Child
      INTEGER :: childId=2
      CLASS(*), POINTER  :: UArr(:) 
    END TYPE

  END MODULE

  PROGRAM SltTypeCompArr
  USE M
  IMPLICIT NONE

  CLASS(*), POINTER :: Ptr(:)
  TYPE(Child), TARGET :: Tar(3)

  Ptr => Tar
  SELECT TYPE (Ptr)
    TYPE IS (Child)
      Ptr(1)%UArr           => Ptr 
      Ptr(1)%Base1%BaseArr1 => Ptr%Base
      Ptr(1)%BaseArr        => Ptr%Base
  END SELECT

  SELECT TYPE ( As => Ptr)
   CLASS DEFAULT
     STOP 19
   CLASS IS (Base)
     STOP 20
   TYPE IS (Base)
     STOP 21
   CLASS IS (Child)
     STOP 22
   TYPE IS (Child)
    
     IF ( ANY(As%BaseId  .NE. -1  ))  STOP 31
     IF ( ANY(As%Base1Id .NE.  1  ))  STOP 32
     IF ( ANY(As%ChildId .NE.  2  ))  STOP 33

     IF (ANY(As(1)%Base1%BaseArr1%BaseId .NE. -1) )  STOP 41
     IF (ANY(As(1)%Base%BaseArr%BaseId   .NE. -1) )  STOP 42

     SELECT TYPE(As1 => As(1)%UArr)
       TYPE IS (Child)
         IF ( ANY(As1%BaseId .NE. -1) ) STOP 51
         IF ( ANY(As1%Base1Id .NE. 1) ) STOP 52
         IF ( ANY(As1%ChildId .NE. 2) ) STOP 53
       CLASS DEFAULT
         STOP 54
     END SELECT

  END SELECT

  END
