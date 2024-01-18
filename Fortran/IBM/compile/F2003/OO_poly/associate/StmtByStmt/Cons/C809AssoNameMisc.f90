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
! %POSTCMD: tcomp C809AssoNameMisc.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : C809AssoNameMisc
!*
!*  DATE                       : Oct. 20, 2004
!*
!*  PRIMARY FUNCTIONS TESTED   : Associate
!*
!*  SECONDARY FUNCTIONS TESTED : Associate name
!*
!*  REFERENCE                  : Feature 219934
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*     Wrong associate construct with the same associate name
!*     (ICE 294717)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



    ASSOCIATE ( As => 1  )
      ASSOCIATE ( As => 2 )
      END ASSOCIATE
  END

