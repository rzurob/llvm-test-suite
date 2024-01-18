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
! %POSTCMD: tcomp PtrAssignProcNameIntrin7.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : PtrAssignProcNameIntrin7
!*
!*  DATE                       : Mar. 14, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure pointer
!*
!*  SECONDARY FUNCTIONS TESTED : Pointer assignment
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
!*  C727 (R742) A procedure-name shall be the name of an external, module,
!*  or dummy procedure, a specific intrinsic function listed in 13.6
!*  and not marked with a bullet (.), or a procedure pointer.
!*
!*  (315369)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM PtrAssignProcNameIntrin7
  IMPLICIT NONE

  TYPE :: DT
    PROCEDURE(LOGICAL),  POINTER, NOPASS :: PtrLGE
    PROCEDURE(LOGICAL),  POINTER, NOPASS :: PtrLGT
    PROCEDURE(LOGICAL),  POINTER, NOPASS :: PtrLLE
    PROCEDURE(INTEGER),  POINTER, NOPASS :: PtrMAX0
    PROCEDURE(INTEGER),  POINTER, NOPASS :: PtrMAX1
    PROCEDURE(INTEGER),  POINTER, NOPASS :: PtrMIN0
    PROCEDURE(INTEGER),  POINTER, NOPASS :: PtrMIN1
    PROCEDURE(REAL),     POINTER, NOPASS :: PtrREAL
    PROCEDURE(REAL),     POINTER, NOPASS :: PtrSNGL
  END TYPE

  INTRINSIC LGE, LGT, LLE, MAX0, MAX1, MIN0, MIN1, REAL, SNGL

  TYPE (DT) :: V

  V%PtrLGE => LGE

  V%PtrLGT => LGT

  V%PtrLLE => LLE

  V%PtrMAX0 => MAX0

  V%PtrMAX1 => MAX1

  V%PtrMIN0 => MIN0

  V%PtrMIN1 => MIN1

  V%PtrREAL => REAL

  V%PtrSNGL => SNGL

  END

