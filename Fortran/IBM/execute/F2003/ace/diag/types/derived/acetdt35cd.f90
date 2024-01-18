!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : acetdt35cd
!*
!*  DATE                       : 2006-11-16
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : print DT lacking user-defined print routine where it is required
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : DTIO
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Attempt to print DT which needs user-defined DTIO routine, but does not have
!*  one.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module acetdt35cdmod

  implicit none
  type Contained
     double precision :: dp
     double complex   :: dz
  end type Contained

  type ADerived
     integer, allocatable :: iafield (:)
     complex, allocatable :: zafield
     type(Contained), allocatable :: dafield
     type(Contained), allocatable :: daArray(:)
     real, pointer :: rpfield
     character, pointer :: cpfield(:)
     type(Contained), pointer :: dpfield
     type(Contained), pointer :: dpArray(:)
  end type ADerived

end module acetdt35cdmod


program acetdt35cd

  use acetdt35cdmod
  implicit none

  ! Verify that an error message is generated if DTIO is not defined for certain kinds of derived type (i.e., those with
  ! allocatable or pointer components, or components which contain these but have no DTIO defined)

  print *, [ADerived:: ADerived(null(),null(),null(),null(),null(),null(),null(),null())]
  print *, [ADerived::]

end program acetdt35cd
