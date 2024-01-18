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
! %POSTCMD: tcomp PtrAssignProcNameIntrin6.f 
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : PtrAssignProcNameIntrin6.f 
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Mar. 14, 2005
!*  ORIGIN                     : AIX Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure pointer 
!*
!*  SECONDARY FUNCTIONS TESTED : Pointer assignment 
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
!*  C727 (R742) A procedure-name shall be the name of an external, module,
!*  or dummy procedure, a specific intrinsic function listed in 13.6
!*  and not marked with a bullet (.), or a procedure pointer.
!* 
!*  Intrinsics
!*  (315369/315826) 
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM PtrAssignProcNameIntrin6
  IMPLICIT NONE

  PROCEDURE(REAL),             POINTER :: PtrAMAX0
  PROCEDURE(REAL),             POINTER :: PtrAMAX1
  PROCEDURE(REAL),             POINTER :: PtrAMIN0
  PROCEDURE(REAL),             POINTER :: PtrAMIN1
  PROCEDURE(CHARACTER(1)),     POINTER :: PtrCHAR
  PROCEDURE(DOUBLE PRECISION), POINTER :: PtrDMAX1
  PROCEDURE(DOUBLE PRECISION), POINTER :: PtrDMIN1
  PROCEDURE(REAL),             POINTER :: PtrFLOAT
  PROCEDURE(INTEGER),          POINTER :: PtrICHAR
  PROCEDURE(INTEGER),          POINTER :: PtrIDINT
  PROCEDURE(INTEGER),          POINTER :: PtrIFIX
  PROCEDURE(INTEGER),          POINTER :: PtrINT

  INTRINSIC AMAX0, AMAX1, AMIN0, AMIN1, CHAR, DMAX1, DMIN1, FLOAT, ICHAR, IDINT, IFIX, INT

  PtrAMAX0 => AMAX0

  PtrAMAX1 => AMAX1

  PtrAMIN0 => AMIN0

  PtrAMIN1 => AMIN1

  PtrCHAR  => CHAR

  PtrDMAX1 => DMAX1

  PtrDMIN1 => DMIN1

  PtrFLOAT => FLOAT

  PtrICHAR => ICHAR

  PtrIDINT => IDINT

  PtrIFIX  => IFIX

  PtrINT   => INT
 
  END

