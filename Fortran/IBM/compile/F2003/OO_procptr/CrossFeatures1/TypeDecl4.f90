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
! %POSTCMD: tcomp TypeDecl4.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             :  TypeDecl4.f
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
!*  If the VALUE attribute is specified, the EXTERNAL
!*  shall not be specified.
!*
!*  (ICE-304817)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  PROGRAM TypeDecl4

! Stop Compilation !

  CONTAINS

  SUBROUTINE IntSub(Proc, ProcPtr)

  VALUE        :: Proc
  PROCEDURE()  :: Proc

  VALUE                 :: ProcPtr
  PROCEDURE(), POINTER  :: ProcPtr

  END SUBROUTINE

  END


