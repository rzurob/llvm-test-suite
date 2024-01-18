!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParameterInquiryResParam04.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : July 29 2008 
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : TYPE PARAMETER INQUIRY
!*
!*  SECONDARY FUNCTIONS TESTED :  
!*
!*  REFERENCE                  : 
!*
!*  DRIVER STANZA              : xlf2003
!*
!*
!*  DESCRIPTION
!*
!* 1. TEST SECTION 6.1.3 
!* 2. TYPE PARAMETER INQUIRY
!* 3. FUNCTION RESULT AS ACTUAL ARGUMENT
!* 4. DEFECT 354454
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type base(l1)
     integer(2),len :: l1=4
   end type

   type,extends(base) :: child(l2)
     integer(8),len :: l2=6
   end type

   contains

     function getlen(dt)
        class(base(*)) :: dt
        integer :: getlen
        select type(dt)
          type is(child(*,*))
             getlen=dt%l1+dt%l2
          type is(base(*))
             getlen=dt%l1
        end select
     end function
     
     function getlenkind(dt)
        class(base(*)) :: dt
        integer :: getlenkind
        select type(dt)
          type is(child(*,*))
             getlenkind=dt%l1%kind+dt%l2%kind
          type is(base(*))
             getlenkind=dt%l1%kind
          class default
             error stop 100_4
        end select         
     end function

end module

  program dtParameterInquiryResParam04 
  use m
  implicit none

  interface
  subroutine setdt(dt,length)
     import 
     type(base(:)),pointer,intent(inout) :: dt
     integer :: length
  end subroutine

  function setchar(dt,length)
      import
      type(base(*)) :: dt
      character(dt%l1+length) setchar
      integer :: length
  end function 
  end interface

  type(child) :: c1
  type(base)  :: b1
  type(base(:)),pointer :: p1
  character(:),allocatable :: a1

  allocate(base(getlen(b1)) :: p1)
  call setdt(p1,getlen(c1))   
  if(p1%l1 /= 10)                                        error stop 11_4
  
  allocate(a1,source=setchar(p1,3))
  if(a1 /='xlftest' )                                    error stop 12_4
  if(a1%len /= len(a1) .or. a1%len /= 13)                error stop 13_4
  
  deallocate(a1)
  allocate(a1,source=setchar(p1,getlen(c1)))
  if(a1 /= 'xlftest')                                     error stop 14_4
  if(a1%len /=len(a1) .or. a1%len /= 20)                  error stop 15_4

  if(getkind(getlenkind(p1)) /= p1%l1%kind)               error stop 16_4
  if(getkind(getlenkind(c1)) /= c1%l1%kind+c1%l2%kind)    error stop 17_4
  if(getkind(getlenkind(b1)) /= b1%l1%kind)               error stop 18_4

!  print *,p1%l1%kind,b1%l1%kind,c1%l1%kind+c1%l2%kind
!  print *,getkind(getlenkind(p1)),getkind(getlenkind(c1)),getkind(getlenkind(b1))
  contains
     integer function getkind(lenkind)
         integer :: lenkind
         getkind=lenkind
     end function
end

  subroutine setdt(dt,length)
     use m
     type(base(:)),pointer,intent(inout) :: dt
     integer :: length 
     if(dt%l1 /= 4)                                      error stop 10_4
     allocate(base(length) :: dt)
  end subroutine 

  function setchar(dt,length)
      use m
      type(base(*)) :: dt
      character(dt%l1+length) setchar
      integer :: length
      setchar='xlftest'    
  end function   

