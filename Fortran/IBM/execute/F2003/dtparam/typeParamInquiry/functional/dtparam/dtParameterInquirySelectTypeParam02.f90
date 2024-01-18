!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParameterInquirySelectTypeParam02.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : July 23 2008 
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
!* 3. SELECT TYPE IN SUBROUTINE
!* 4. DUMMY ARGUMENT IS ASSUMED 
!* 5. CALL THROUGH MULTIPLE SUBROUTINE
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type ::base(k1,l1)
      integer(1),kind :: k1=2
      integer(8),len  :: l1=3
   end type

   type,extends(base)  :: child(k2,l2)
      integer(2),kind  :: k2=4
      integer(4),len   :: l2=5
   end type
     
   contains
     subroutine checktype1(dt)
         class(base(2,*)) :: dt
         select type(dt)
            type is(base(2,*))
               print *,"checktype1 : dt is base" 
               if(dt%k1 /=2)                                  error stop 9_4
               if(dt%l1 /=3)                                  error stop 10_4
               if(dt%k1%kind /=kind(dt%k1) &
                    .or. dt%k1%kind /= 1)                     error stop 11_4
               if(dt%l1%kind /=kind(dt%l1) & 
                   .or. dt%l1%kind /= 8)                      error stop 12_4

            type is(child(2,*,4,*))
               print *,"checktype1 : dt is child"
               if(dt%k1 /=2)                                  error stop 13_4
               if(dt%l1 /=3)                                  error stop 14_4
               if(dt%k1%kind /=kind(dt%k1)  &
                    .or. dt%k1%kind /= 1)                     error stop 15_4
               if(dt%l1%kind /=kind(dt%l1)  &
                .or. dt%l1%kind /= 8)                         error stop 16_4

               if(dt%k2 /=4)                                  error stop 17_4
               if(dt%l2 /=5)                                  error stop 18_4
               if(dt%k2%kind /=kind(dt%k2) &
                    .or. dt%k2%kind /= 2)                     error stop 19_4
               if(dt%l2%kind /=kind(dt%l2)  &
                     .or. dt%l2%kind /= 4)                    error stop 20_4

            class is(base(2,*))
               error stop 21_4
            class default
               error stop 22_4
         end select

     end subroutine

     subroutine checktype2(dt)
         class(base(2,*)),intent(in) :: dt
         print *,"checktype2:"
         call checktype1(dt)
     end subroutine

end module

  program dtParameterInquirySelectTypeParam02 
  use m
  implicit none
  
  interface
     subroutine checktype3(dt)
         import
         class(base(2,*)),intent(in) :: dt
     end subroutine

  end interface

  type(base),target   :: b1
  type(child),target  :: c1
  class(base(2,:)),allocatable :: b2
  class(base(2,:)),pointer     :: b3=>null()

  call checktype1(b1)
  call checktype1(c1)


  allocate(base(2,len('abc')) :: b2)
  call checktype2(b2)
  deallocate(b2)
  allocate(child(b1%k1,b1%l1,c1%k2,c1%l2) :: b2)
  call checktype2(b2)

  allocate(b3,source=b1)  
  call checktype3(b3)
  b3=>c1
  call checktype3(b3) 

end

  subroutine checktype3(dt)
      use m
      class(base(2,*)),intent(in) :: dt
      print *,"checktype3:"
      call checktype2(dt)
  end subroutine

