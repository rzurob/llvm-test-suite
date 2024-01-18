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
! %POSTCMD:  tcomp AssocNameAsOther10.f
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : AssocNameAsOther10 
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Mar. 01, 2005
!*  ORIGIN                     : AIX Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Associate
!*
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  REFERENCE                  : Feature 219934
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*    The associate selector is the same as a type name 
!*   (300666-confusing err msg) 
!*
!234567890123456789012345678901234567890123456789012345678901234567890

 

  PROGRAM AssocNameAsOther10
  IMPLICIT NONE

  TYPE :: DT
    INTEGER :: i
  END TYPE

  INTEGER :: i
  COMMON /Cmblk/ i
 
  ASSOCIATE ( DT => i )
  END ASSOCIATE 

  ASSOCIATE ( A => DT )
  END ASSOCIATE 

  ASSOCIATE ( A => AssocNameAsOther10 )
  END ASSOCIATE 

  ASSOCIATE ( A => Sub )
  END ASSOCIATE 

  ASSOCIATE ( A => Cmblk )
  END ASSOCIATE 

  CONTAINS
  SUBROUTINE Sub()
  END SUBROUTINE 

  END

