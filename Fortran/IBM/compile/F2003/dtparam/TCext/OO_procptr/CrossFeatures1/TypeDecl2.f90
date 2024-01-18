! GB DTP extension using:
! ftcx_dtp -qck /tstdev/OO_procptr/CrossFeatures1/TypeDecl2.f
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
! %POSTCMD: tcomp TypeDecl2.f
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             :  TypeDecl2.f
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jun. 07, 2005
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
!*  
!*  The PROTECTED attribute is permitted only for a procedure pointer 
!*  or named variable that is not in a common block.  
!*  
!*  After the new C536 approved, the TC should be changed! 
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

  TYPE :: Base(K1,N1)    ! (1,3)
    INTEGER, KIND             :: K1
    INTEGER, LEN              :: N1
    CHARACTER(kind=K1,len=N1) :: C
  END TYPE

  TYPE(Base(1,3))   :: ExtFun
  PROCEDURE()  :: ExtFun
  PROTECTED    :: ExtFun

  TYPE(Base(1,3))            :: ProcPtr 
  PROCEDURE(), POINTER  :: ProcPtr 
  PROTECTED             :: ProcPtr ! Should be ok after new C536 

  PROCEDURE(),          PROTECTED :: ProcPtr1
  PROCEDURE(), POINTER, PROTECTED :: ProcPtr2  ! Should be ok after new C536

  END MODULE


  PROGRAM TypeDecl2 
  Stop Compilation ! 
  END


