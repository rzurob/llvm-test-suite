! GB DTP extension using:
! ftcx_dtp -qck /tstdev/OO_procptr/CrossFeatures1/Intent1.f
! opt variations: -qnock

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: redherring.f 
! %VERIFY:  
! %STDIN:
! %STDOUT: 
! %EXECARGS:
! %POSTCMD: tcomp Intent1.f
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : Intent1.f 
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : May. 26, 2005
!*  ORIGIN                     : AIX Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure pointer 
!*
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  REFERENCE                  : Feature 289058 
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
!*  Intent - INTENT(OUT)/INTENT(INOUT) 
!*  
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE :: Base(K1,N1)    ! (1,3)
      INTEGER, KIND             :: K1
      INTEGER, LEN              :: N1
      CHARACTER(kind=K1,len=N1) :: C
    END TYPE
 
    IMPLICIT TYPE(Base(1,3))(P)

    INTERFACE
      SUBROUTINE IntF(Arg1, Arg2)
      IMPORT
        TYPE(Base(1,*)), INTENT(IN)  :: Arg2 
        TYPE(Base(1,*)), INTENT(OUT) :: Arg1 
      END SUBROUTINE 
    END INTERFACE

  CONTAINS

    SUBROUTINE ModSub1(ProcPtr) 
    PROCEDURE(IntF), POINTER, INTENT(IN) :: ProcPtr
      CALL ModSub4(ProcPtr)
      CALL ModSub5(ProcPtr)
    END SUBROUTINE 

    SUBROUTINE ModSub2(ProcPtr) 
    PROCEDURE(IntF), POINTER, INTENT(IN) :: ProcPtr
      CALL ModSub4(ProcPtr)
      CALL ModSub5(ProcPtr)
    END SUBROUTINE 

    SUBROUTINE ModSub3(ProcPtr) 
    IMPLICIT TYPE(Base(1,3))(P)
    PROCEDURE(IntF), POINTER, INTENT(IN) :: ProcPtr
      CALL ModSub4(ProcPtr)
      CALL ModSub5(ProcPtr)
    END SUBROUTINE 

    SUBROUTINE ModSub4(ProcPtr) 
    PROCEDURE(IntF), POINTER, INTENT(INOUT) :: ProcPtr
    END SUBROUTINE 

    SUBROUTINE ModSub5(ProcPtr) 
    PROCEDURE(IntF), POINTER, INTENT(OUT) :: ProcPtr
    END SUBROUTINE 

  END MODULE


  PROGRAM Intent1 
  END

