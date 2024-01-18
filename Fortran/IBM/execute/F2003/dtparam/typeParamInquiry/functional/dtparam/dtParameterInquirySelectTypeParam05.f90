!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParameterInquirySelectTypeParam05.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : July 24 2008 
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
!* 3. DUMMY ARGUMENT IS INTENT(OUT) ALLOCATABLE OR POINTER 
!234567890123456789012345678901234567890123456789012345678901234567890
module m
       type base(k1,l1)
          integer,kind :: k1
          integer(k1%kind),len :: l1         
       end type
       
       type,extends(base) :: child(k2,l2)
          integer(k1%kind),kind :: k2    
          integer(k2),len :: l2
       end type
      contains

      subroutine check3(dt1,dt2)
       class(base(2,:)),pointer,intent(out)  :: dt1
       type(child(2,*,4,*)),target,intent(in) :: dt2

       print *,"call check3"
       dt1=>dt2
       end subroutine
end module

  program dtParameterInquirySelectTypeParam05 
  use m
  implicit none

  interface
    subroutine check1(dt)
       import
       class(base(2,:)),allocatable,intent(out)  :: dt
    end subroutine

  subroutine check2(dt1,dt2)
     import
     class(base(2,:)),allocatable,intent(out) :: dt1
     type(child(2,*,4,*)),intent(in) :: dt2

  end subroutine

  end interface

  class(base(2,:)),pointer  :: p1 => null()
  class(base(2,:)),allocatable  :: a1
  type(child(a1%k1,2*5,4,3*max(3,5))),target :: a2
  
  call check1(a1)

  select type(a1)
     type is(base(2,*))
        print *,"a1 is base"
        if(a1%k1 /= 2)                                   error stop 10_4
        if(a1%l1 /= 5)                                   error stop 11_4
        if(a1%k1%kind /=kind(a1%k1) .or. &
              a1%k1%kind /= 4)                           error stop 12_4
        if(a1%l1%kind /=kind(a1%l1) .or. &
              a1%l1%kind /= 4)                           error stop 13_4
  end select

  call check2(a1,a2)

  select type(a1)
     type is(child(2,*,4,*))
       print *,"a1 is child"
       if(a1%k1 /= 2)                                   error stop 14_4
       if(a1%l1 /= 10)                                  error stop 15_4
       if(a1%k1%kind /=kind(a1%k1) .or. &
            a1%k1%kind /= 4)                            error stop 16_4
       if(a1%l1%kind /=kind(a1%l1) .or. &
            a1%l1%kind /= 4)                            error stop 17_4

       if(a1%k2 /= 4)                                   error stop 18_4
       if(a1%l2 /= 15)                                  error stop 19_4
       if(a1%k2%kind /=kind(a1%k2) .or.  &
           a1%k2%kind /= 4)                             error stop 20_4
       if(a1%l2%kind /=kind(a1%l2) .or. &
           a1%l2%kind /= 4)                             error stop 21_4

  end select   


  call check3(p1,a2)

  select type(p1)
     type is(child(2,*,4,*))
       print *,"p1 is child"
       if(p1%k1 /= 2)                                   error stop 22_4
       if(p1%l1 /= 10)                                  error stop 23_4
       if(p1%k1%kind /=kind(p1%k1) .or. &
            p1%k1%kind /= 4)                            error stop 24_4
       if(p1%l1%kind /=kind(p1%l1) .or. &
            p1%l1%kind /= 4)                            error stop 25_4

       if(p1%k2 /= 4)                                   error stop 26_4
       if(p1%l2 /= 15)                                  error stop 27_4
       if(p1%k2%kind /=kind(p1%k2) .or.  &
           p1%k2%kind /= 4)                             error stop 28_4
       if(p1%l2%kind /=kind(p1%l2) .or. &
           p1%l2%kind /= 4)                             error stop 29_4

  end select

end

  subroutine check1(dt)
     use m
     class(base(2,:)),allocatable,intent(out) :: dt

     print *,"call check1"
     allocate(base(2,max(3,5)) :: dt)
  end subroutine

  subroutine check2(dt1,dt2)
     use m
     class(base(2,:)),allocatable,intent(out) :: dt1
     type(child(2,*,4,*)),intent(in) :: dt2

     print *,"call check2"
     allocate(dt1,source=dt2)
  end subroutine
