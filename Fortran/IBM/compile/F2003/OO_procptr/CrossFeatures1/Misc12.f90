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
!*
!*  TEST CASE NAME             :  Misc11.f
!*
!*  DATE                       : Jun. 08, 2005
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
!*  ASSOCIATE/SELECT TYPE
!*
!*  (306669)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM Misc12
  IMPLICIT NONE

  TYPE :: DT
    PROCEDURE(), POINTER, NOPASS :: ProcPtr => NULL()
  END TYPE

  PROCEDURE(TYPE(DT)), POINTER :: ProcPtr
  PROCEDURE(),         POINTER :: ProcPtr1

  TYPE(DT)  :: V

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


