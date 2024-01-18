!**********************************************************************
!* ====================================================================
!*
!*  DATE                       : 2007-12-07
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : Array Constructor Enhancement
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*
!*  DEFECT ABSTRACT            : DTPARAM: ICE in ASTI in op_conv__Fi
!*
!*  DESCRIPTION                :
!*  When Compiled, the Reduced Code below causes an ICE in ASTI (refer to
!*  the stack trace below).
!*
!*  The ICE disappears if the Length Type Parameter specified for the "dt"
!*  Derived Type Component of the Type "sContainer" is replaced with a
!*  specific value (eg: 4).
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

module d344637mod

  type dt (ldt_1) ! ldt_1=4
     integer, len :: ldt_1
     integer(4) :: datum

   contains

     procedure :: dtAssignDt
     generic   :: assignment(=) => dtAssignDt
  end type dt

  type sContainer (lsContainer_1) ! ksContainer_1,lsContainer_1=4,4
     integer, len :: lsContainer_1
     type (dt(lsContainer_1)) :: d ! tcx: (ksContainer_1)    <==
                                   ! ICE disappears if "lsContainer_1"
                                   ! is Replaced with "4"
  end type sContainer

contains

  elemental subroutine dtAssignDt(this, that)
    class (dt(*)), intent(inout) :: this ! tcx: (*)
    class (dt(*)), intent(in) :: that ! tcx: (*)

    this % datum = that % datum
  end subroutine dtAssignDt

end module d344637mod

program d344637
  use d344637mod

  type (sContainer(4)) :: sc ! tcx: (4,4)

end program d344637
