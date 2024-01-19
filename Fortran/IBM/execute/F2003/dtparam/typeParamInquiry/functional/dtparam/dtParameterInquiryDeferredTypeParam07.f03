!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : July 25 2008
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
!* 2. TYPE PARAMETER INQUIRY
!* 3. WITH CHARACTER COMPONENT
!* 4. USE POLYMORPHIC
!* 5. ASSUMED AND DEFFERED DERIVED TYPE
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type base(l1)
      integer(2),len :: l1
      character(len=l1) :: c1(l1-1:l1+1)
      contains
         procedure,pass :: checkbase =>getbase
   end type
   type,extends(base) :: child(l2)
      integer(8),len :: l2
      character(len=l1+l2) :: c2(l1-l2:l1+l2)
      contains
         procedure,pass  :: checkchild =>getchild
   end type

   contains
       subroutine getbase(b)
          class(base(*)),intent(in) :: b
          print *,"base info"
          print *,b%l1
          print *,b%l1%kind,kind(b%l1)
          print *,b%c1%len,len(b%c1)
          print *,lbound(b%c1,1),ubound(b%c1,1)
       end subroutine

       subroutine getchild(c)
          class(child(*,*)),intent(in) :: c
          print *,"child info"
          print *,c%l2
          print *,c%l2%kind,kind(c%l2)
          print *,c%c2%len,len(c%c2)
          print *,lbound(c%c2,1),ubound(c%c2,1)
       end subroutine

       subroutine getDT(dt)
          class(base(:)),pointer,intent(in) :: dt
          print *,"check DT"
          select type(dt)
             type is(base(*))
                  call dt%checkbase
             type is(child(*,*))
                  call dt%checkchild
          end select
       end subroutine

end module

  program dtParameterInquiryDeferredTypeParam07
  use m
  implicit none

  type(base(3)),target :: b1
  type(child(3,5)),target :: c1
  class(base(:)),pointer :: dt1
  call getbase(b1)
  call getchild(c1)
  dt1=>b1
  call getDT(dt1)
  dt1=>c1
  call getDT(dt1)

end
