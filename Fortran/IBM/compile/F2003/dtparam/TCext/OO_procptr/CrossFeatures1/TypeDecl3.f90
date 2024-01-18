! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_procptr/CrossFeatures1/TypeDecl3.f
! opt variations: -ql

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
! %POSTCMD: tcomp TypeDecl3.f
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             :  TypeDecl3.f
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
!*  If the VOLATILE attribute is specified, the PARAMETER,  
!*  INTRINSIC, EXTERNAL, or INTENT(IN) attribute shall not be specified.  
!*  
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  PROGRAM TypeDecl3 

  INTEGER      :: ExtFun
  PROCEDURE()  :: ExtFun
  VOLATILE     :: ExtFun

  INTEGER               :: ProcPtr 
  PROCEDURE(), POINTER  :: ProcPtr 
  VOLATILE              :: ProcPtr 

  TYPE :: DT(K1)    ! (4)
    INTEGER, KIND :: K1
    INTEGER(K1)   :: I
  END TYPE
  VOLATILE :: DT

  PROCEDURE(TYPE(DT(4))), POINTER :: ProcPtr1  !ok

  Stop Compilation ! 
  END


