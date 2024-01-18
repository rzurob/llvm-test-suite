! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/OO_procptr/CrossFeatures2/PtrAssignProcNameIntrin7.f
! opt variations: -qnok -qnol

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
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : PtrAssignProcNameIntrin7 
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
!* 
!*  (315369) 
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM PtrAssignProcNameIntrin7
  IMPLICIT NONE
  
  TYPE :: DT(K1,N1)    ! (4,20)
    INTEGER, KIND :: K1
    INTEGER, LEN  :: N1
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
 
  TYPE (DT(4,20)) :: V

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

