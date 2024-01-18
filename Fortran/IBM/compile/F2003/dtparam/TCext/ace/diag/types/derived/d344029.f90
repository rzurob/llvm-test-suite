!**********************************************************************
!* ====================================================================
!*
!*  DATE                       : 2007-11-20
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : Array Constructor Enhancement
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*
!*  DEFECT ABSTRACT            : DTPARAM: Unexpected Diagnostics for Structure
!*                               Constructor with Length Type Parameter
!*
!*  DESCRIPTION                :
!*  The Reduced Code below should successfully Compile.  However, the
!*  compiler emits 2 Diagnostic Messages for Line 10:
!*
!*  line 10.30: 1514-090 (S) Wrong number of arguments specified for structure constructor.
!*  line 10.33: 1516-050 (S) Expression or initial value must be evaluated at compile time.
!*
!*  Both messages refer to the Structure Constructor used for default
!*  initialization.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

module d344029mod

  type :: dt(l1)    ! (20,4,4)
     integer, len  :: l1
     integer(4)   :: field1 = -999
  end type dt

  type :: modtype(l2)    ! (4,20)
     integer, len       :: l2
     type(dt(l2)) :: myval! = dt(l2)(-999)   ! <= Line 10
  end type modtype

  interface
     type(dt(20)) function myabs(this)
       import modtype, dt
       class(modtype(*)), intent(in) :: this
     end function myabs
  end interface

end module d344029mod
