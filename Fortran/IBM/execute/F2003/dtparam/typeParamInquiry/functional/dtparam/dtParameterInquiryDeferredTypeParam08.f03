!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : July 24 2008
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
!* 3. WITHOUT COMPONENT
!* 4. DUMMY ARGUMENT IS INTENT(INOUT) ALLOCATABLE OR POINTER DERIVED TYPE
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type ::base(k,l)
      integer(2),kind :: k
      integer(8),len  :: l
   end type

   contains

     subroutine setTP1(b1)
        type(base(4,:)),allocatable,intent(inout) :: b1
        print *,"in setTP1"
        if(allocated(b1))  then
           print *,b1%k,b1%k%kind,kind(b1%k)
           print *,b1%l,b1%l%kind,kind(b1%l)
           print *,"reallocate"
           deallocate(b1)
           allocate(base(4,len("abc")) :: b1)
        else
           print *,"not allocated"
           return
        endif
     end subroutine

     subroutine setTP2(b2)
        type(base(2,:)),pointer,intent(inout) :: b2

        print *,"in setTP2"
        if(associated(b2)) then
           print *,b2%k,b2%k%kind,kind(b2%k)
           print *,b2%l,b2%l%kind,kind(b2%l)
           print *,"reallocate"
           deallocate(b2)
           allocate(base(1+1,len("xlftest")+len("xlf")) :: b2)
        else
           print *,"not associated"
           return
        endif
     end subroutine

end module

  program dtParameterInquiryDeferredTypeParam08
  use m
  implicit none

  type(base(4,:)),allocatable :: b1
  type(base(2,:)),pointer  :: b2=>null()

  call setTP1(b1)
  allocate(base(4,10) :: b1)
  call setTP1(b1)
  print *,b1%k,b1%k%kind,kind(b1%k)
  print *,b1%l,b1%l%kind,kind(b1%l)

  call setTP2(b2)
  allocate(base(2,int(3.3)) :: b2)
  call setTP2(b2)
  print *,b2%k,b2%k%kind,kind(b2%k)
  print *,b2%l,b2%l%kind,kind(b2%l)

end
