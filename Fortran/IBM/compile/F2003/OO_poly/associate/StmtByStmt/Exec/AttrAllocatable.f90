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
! %POSTCMD: tcomp AttrAllocatable.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : AttrAllocatable
!*
!*  DATE                       : Feb 22, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Associate
!*
!*  SECONDARY FUNCTIONS TESTED :
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
!*
!*   The associating entity does not have the allocatable attribute
!*
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM AttrAllocatable

  CONTAINS
  SUBROUTINE Sub(A)
  CLASS(*), ALLOCATABLE :: A

    ASSOCIATE ( A => A)
      ALLOCATE( INTEGER::A)
      PRINT*, ALLOCATED(A)
      DEALLOCATE( A )
    END ASSOCIATE

  END SUBROUTINE
  END
