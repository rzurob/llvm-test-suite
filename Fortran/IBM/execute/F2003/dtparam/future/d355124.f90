!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : August 17 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : TYPE PARAMETER INQUIRY
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*
!* 1. TEST SECTION 6.1.3
!* 2. DEFECT 355124
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type base(k,l)
      integer(1),kind :: k=2
      integer(k),len  :: l=k

      character(l%kind)  :: c1(l%kind)
      character(kind(l)) :: c2(kind(l))
   end type
end module

program d355124

  use m
  implicit none
  type(base)       :: t

  print *,t%l%kind,kind(t%l),t%l
  print *,t%c1%len,len(t%c1),ubound(t%c1,1)
  print *,t%c2%len,len(t%c2),ubound(t%c2,1)

end
