! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/OO_procptr/CrossFeatures1/Misc12.f
! opt variations: -qnok -qnol

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP:  redherring.f 
! %VERIFY:  
! %STDIN:
! %STDOUT: 
! %EXECARGS: 
! %POSTCMD:  tcomp Misc12.f 
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             :  Misc11.f
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jun. 08, 2005
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
!*  ASSOCIATE/SELECT TYPE 
!*  
!*  (306669) 
!*  
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM Misc12
  IMPLICIT NONE 

  TYPE :: DT(K1,N1)    ! (4,20)
    INTEGER, KIND :: K1
    INTEGER, LEN  :: N1
    PROCEDURE(), POINTER, NOPASS :: ProcPtr => NULL()
  END TYPE

  PROCEDURE(TYPE(DT(4,20))), POINTER :: ProcPtr
  PROCEDURE(),               POINTER :: ProcPtr1

  TYPE(DT(4,20))  :: V

  ASSOCIATE ( As => ProcPtr )
  END ASSOCIATE

  ASSOCIATE ( As => V%ProcPtr )
  END ASSOCIATE

  ASSOCIATE ( As => ProcPtr1 )
  END ASSOCIATE

  ASSOCIATE ( As => ProcPtr() )
  END ASSOCIATE

! SELECT TYPE (As => ProcPtr)
! END SELECT

! SELECT TYPE (As => V%ProcPtr)
! END SELECT

  END


