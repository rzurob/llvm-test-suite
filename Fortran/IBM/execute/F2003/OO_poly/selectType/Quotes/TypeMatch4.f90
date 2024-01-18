! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: TypeMatch4.f
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
!*  TEST CASE NAME             : TypeMatch4
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jan. 24, 2005
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
!*   The nearest type matchs that of selector
!*    Multiple CLASS IS clauses 
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M

    TYPE  :: Zero
    END TYPE 

    TYPE, EXTENDS(Zero)  :: One 
    END TYPE

    TYPE, EXTENDS(One) :: Two 
    END TYPE

    TYPE, EXTENDS(Two) :: Three
    END TYPE

  END MODULE

  PROGRAM TypeMatch4 
  USE M,  Three=>One, One=>Three 
  IMPLICIT NONE
  CLASS(*), ALLOCATABLE :: U(:,:)
   
    ALLOCATE(Two :: U(2:3,3:4) )

    SELECT TYPE (U)
    CLASS IS (Zero) 
       STOP 40
    TYPE IS (Three) 
       STOP 41
    TYPE IS (One)
       STOP 47
    CLASS IS (Three)

      IF ( SIZE(U)          .NE. 4 )          STOP 31
      IF ( ANY (LBOUND(U)   .NE. (/2, 3/) ) ) STOP 32
      IF ( ANY (UBOUND(U)   .NE. (/3, 4/) ) ) STOP 33
      IF ( ANY(SHAPE(U)     .NE. (/2,2/)) )   STOP 34
       
    CLASS DEFAULT
       STOP 46
    END SELECT
 
  END



