!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-10-31
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : Abstract Interface
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*
!*  DEFECT ABSTRACT            : DTPARAM: DIAG: PROCPTR: Declared with Explicit
!*                               Interface; TARGET must also be Declared with
!*                               Explicit Interface
!*
!*  DESCRIPTION                :
!*  Procedure Pointer Component of a Derived Type (with Type Parameters)
!*  is the TARGET for a Procedure Pointer Assignment.  Compiler emits
!*  the Diagnostic:
!*
!*  1515-124 (S) The procedure pointer was declared with an explicit interface.
!*  The procedure pointer assignment target must also be declared with an
!*  explicit interface.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

  interface
    function one()
      integer one
    end function one
  end interface

  type dt (ldt_1) ! kdt_1,ldt_1=4,13
    integer, len  :: ldt_1
    procedure (one), pointer, nopass :: dtp
  end type dt

end module m

program d343324

  use m

  procedure (one), pointer :: pp

  type(dt(13)) :: dta ! tcx: (4,13)

  dta%dtp => one
  pp => dta%dtp     ! <- Line 25 (Defect)/Line 60 (d343324.f)

  print *, "pp      =", pp( )
  print *, "dta%dtp =", dta%dtp( )

end program d343324

function one()
  integer one
  one = 1
end function one
