!**********************************************************************
!* ====================================================================
!*
!*  TEST CASE NAME             : d344324
!*
!*  DATE                       : 2007-11-28
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : Array Constructor Enhancement
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*
!*  DEFECT ABSTRACT            : DTPARAM: ICE: Derived Type with Type Parameters
!*                               in Array Constructor (within an I/O Statement)
!*
!*  DESCRIPTION                :
!*  The Reduced Code below causes the Compiler to ICE in "xlfentry".
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

module d344324mod

  implicit none

  type ADerived (kADerived_2) ! kADerived_1,kADerived_2,kADerived_3,kADerived_4=4,4,4,1
     integer, kind :: kADerived_2
     complex(kADerived_2) :: zafield
  end type ADerived

end module d344324mod

program d344324

  use d344324mod
  implicit none

  print *, [ADerived(4)::] ! tcx: (4,4,4,1)

end program d344324
