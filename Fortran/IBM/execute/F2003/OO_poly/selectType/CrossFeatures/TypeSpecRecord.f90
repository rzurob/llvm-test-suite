! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: TypeSpecRecord.f 
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
!*  TEST CASE NAME             : TypeSpecRecord
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jan. 28, 2005
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
!*  Type Spec : record 
!* 
!*  (ICE-299038) 
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM  TypeSpecRecord
  IMPLICIT NONE

  STRUCTURE /S1/
    STRUCTURE /S2/ A
      INTEGER :: I=1
    END STRUCTURE
    STRUCTURE B
      INTEGER :: J=2
    END STRUCTURE
  END STRUCTURE

  TYPE :: DT
    RECORD /S1/ R
  END TYPE

  CLASS(*), POINTER  :: U(:,:,:)

    ALLOCATE(DT::U(2,2,2))

S1: SELECT TYPE (S2 => U)
    CLASS DEFAULT

S2: SELECT TYPE (U => S2 )
    CLASS DEFAULT 
      STOP 20 
    TYPE IS (DT)

        IF (SIZE(U)       .NE. 8)            STOP 30
        IF (ANY(SHAPE(U)  .NE. (/2,2,2/) ))  STOP 31
        IF (KIND(U%R.A.I)   .NE. 4)  STOP 35
        IF (KIND(U%R.B.J)   .NE. 4)  STOP 36
        IF (ANY(U%R.A.I     .NE. 1)) STOP 33
        IF (ANY(U%R.B.J     .NE. 2)) STOP 34

    END SELECT S2
    END SELECT S1

  END



