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
!*
!*  TEST CASE NAME             :  TypeDecl3.f
!*
!*  DATE                       : Jun. 07, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure pointer
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature 289058
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
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

  TYPE :: DT
    INTEGER :: I
  END TYPE
  VOLATILE :: DT

  PROCEDURE(TYPE(DT)), POINTER :: ProcPtr1  !ok

  Stop Compilation !
  END


