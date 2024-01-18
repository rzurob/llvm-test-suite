!**********************************************************************
!* ====================================================================
!*
!*  DATE                       : 2008-01-15
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : Array Constructor Enhancement
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*
!*  DEFECT ABSTRACT            : DTPARAM: 1517-005 (U) Error in Get_Kind
!*                               (with Procedure Pointer)
!*
!*  DESCRIPTION                :
!*  The Reduced Code below returns an Error in "Get_Kind":
!*
!*  line 26.0: 1517-005 (U) Error in Get_Kind.  Please contact your Service
!*  Representative.  For more information visit:
!*  http://www.ibm.com/support/docview.wss?uid=swg21110810
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

module d345809mod

  type :: dt (kdt_1) ! kdt_1,kdt_2=4,4
     integer, kind :: kdt_1
     integer(kdt_1) :: field1
  end type dt

  type :: modtype (kmodtype_1) ! kmodtype_1,kmodtype_2,lmodtype_1=4,4,7
     integer, kind :: kmodtype_1
     type(dt(kmodtype_1)) :: myval ! tcx: (kmodtype_1,kmodtype_2)
     procedure (type(dt(kmodtype_1))), nopass, pointer :: modp
  end type modtype

end module d345809mod

program d345809
  use d345809mod

  procedure(type(dt(4))) :: externFunc ! tcx: (4,4)
  type(dt(4)) :: instance1 ! tcx: (4,4)
  type (modtype(4)) :: mt ! tcx: (4,4,7)

  instance1 % field1 = 99

  mt % modp => externFunc
  print *, mt % modp(instance1) ! <= Line 26

end program d345809

type(dt(4)) function externFunc(a) ! tcx: (4,4)
  use d345809mod
  type(dt(4)) :: a ! tcx: (4,4)
  externFunc = a ! tcx: (4,4)
end function externFunc
