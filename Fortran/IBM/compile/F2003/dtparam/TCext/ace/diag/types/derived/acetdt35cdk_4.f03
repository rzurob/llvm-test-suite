!***********************************************************************
!* =====================================================================
!*
!*                               by David Forster)
!*  DATE                       : 2007-11-29 (original: 2006-11-16)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters Array Constructor
!*                               Enhancements
!*  SECONDARY FUNCTIONS TESTED : Array Constructor Enhancement print DT
!*                               lacking user-defined print routine where
!*                               it is required
!*
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*                               (original: Feature Number 289053)
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : DTIO
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Attempt to print DT which needs user-defined DTIO routine, but does
!*  not have one.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

module acetdt35cdk_4mod

  implicit none
  type Contained
     double precision :: dp
     double complex   :: dz
  end type Contained

  type ADerived (kADerived_1,kADerived_2,kADerived_3,kADerived_4) ! kADerived_1,kADerived_2,kADerived_3,kADerived_4=4,4,4,1
     integer, kind :: kADerived_1,kADerived_2,kADerived_3,kADerived_4
     integer(kADerived_1), allocatable :: iafield (:)
     complex(kADerived_2), allocatable :: zafield
     type(Contained), allocatable :: dafield
     type(Contained), allocatable :: daArray(:)
     real(kADerived_3), pointer :: rpfield
     character(kADerived_4), pointer :: cpfield(:)
     type(Contained), pointer :: dpfield
     type(Contained), pointer :: dpArray(:)
  end type ADerived

end module acetdt35cdk_4mod


program acetdt35cdk_4

  use acetdt35cdk_4mod
  implicit none

  ! Verify that an error message is generated if DTIO is not defined for certain kinds of derived type (i.e., those with
  ! allocatable or pointer components, or components which contain these but have no DTIO defined)

  print *, [ADerived(4,4,4,1):: ADerived(4,4,4,1)(null(),null(),null(),null(),null(),null(),null(),null())] ! tcx: (4,4,4,1) ! tcx: (4,4,4,1)
  print *, [ADerived(4,4,4,1)::] ! tcx: (4,4,4,1)

end program acetdt35cdk_4


! Extensions to introduce derived type parameters:
! type: ADerived - added parameters (kADerived_1,kADerived_2,kADerived_3,kADerived_4) to invoke with (4,4,4,1)/declare with (4,4,4,1) - 3 changes
