!**********************************************************************
!* ====================================================================
!*
!*  DATE                       : 2008-01-16
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : Array Constructor Enhancement
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*
!*  DEFECT ABSTRACT            : DTPARAM: DTIO: DUMMYARG: Invalid Diagnostic
!*                               for Extended Type
!*  DESCRIPTION                :
!*  When the Reduced Code (below) is compiled, the following Diagnostic
!*  Message is emitted for the DTIO Binding in the Extended Type:
!*
!*  line 13.37: 1514-699 (S) Procedure "derivedprint" must have a nonoptional
!*  dummy argument that corresponds by position in the argument list to a
!*  dummy argument not present in procedure "baseprint", present and type
!*  incompatible, present with different kind type parameters, or present
!*  with a different rank.
!*
!*  When DTP references are removed, this code compiles without errors.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

module mod

  type :: base(k1)    ! (4,20)
      integer, kind :: k1
   contains
     procedure :: printItem => basePrint
     generic :: write(formatted) => printItem
  end type base

  type, extends (base) :: derived    ! (4,20)
   contains
     procedure :: printItem => derivedPrint
     generic :: write(formatted) => printItem ! derivedPrint <= Line 13
  end type derived

contains

  subroutine basePrint(dtv,unit,iotype,vlist,iostat,iomsg)
    class(base(4)), intent(in) :: dtv
    integer, intent(in) :: unit
    character (len=*), intent(in) :: iotype
    integer, intent(in) :: vlist(:)
    integer, intent(out) :: iostat
    character (len=*), intent(inout) :: iomsg
  end subroutine basePrint

  subroutine derivedPrint(dtv,unit,iotype,vlist,iostat,iomsg)
    class(derived(4)), intent(in) :: dtv
    integer, intent(in) :: unit
    character (len=*), intent(in) :: iotype
    integer, intent(in) :: vlist(:)
    integer, intent(out) :: iostat
    character (len=*), intent(inout) :: iomsg
  end subroutine derivedPrint

end module mod
