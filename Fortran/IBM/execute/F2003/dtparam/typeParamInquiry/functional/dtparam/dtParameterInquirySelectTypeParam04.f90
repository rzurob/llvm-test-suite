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
!* 3. DUMMY ARGUMENT IS INTENT(IN) ALLOCATABLE OR POINTER
!* 4. DEFECT 354300,353970
!234567890123456789012345678901234567890123456789012345678901234567890
module m
       type base(k1,l1)
          integer,kind :: k1
          integer(k1%kind),len :: l1
       end type

       type,extends(base) :: child(k2,l2)
          integer,kind :: k2
          integer(k2),len :: l2
       end type
      contains

      subroutine check2(dt)
       class(base(4,:)),pointer,intent(in) :: dt
       print *,"call check2"
       select type(dt)
       type is(base(4,*))
           print *,"dt is base"
           if(dt%k1 /= 4)                                   error stop 22_4
           if(dt%l1 /= 15)                                  error stop 23_4
           if(dt%k1%kind /=kind(dt%k1) .or. &
                dt%k1%kind /= 4)                            error stop 24_4
           if(dt%l1%kind /=kind(dt%l1) .or. &
                dt%l1%kind /= 4)                            error stop 25_4

       type is(child(kind(4_4),*,2,*))
           print *,"dt is child"
           if(dt%k1 /= 4)                                   error stop 26_4
           if(dt%l1 /= 15)                                  error stop 27_4
           if(dt%k1%kind /=kind(dt%k1) .or. &
                dt%k1%kind /= 4)                            error stop 28_4
           if(dt%l1%kind /=kind(dt%l1) .or. &
                dt%l1%kind /= 4)                            error stop 29_4

           if(dt%k2 /= 2)                                   error stop 30_4
           if(dt%l2 /= 10)                                  error stop 31_4
           if(dt%k2%kind /=kind(dt%k2) .or.  &
                dt%k2%kind /= 4)                            error stop 32_4
           if(dt%l2%kind /=kind(dt%l2) .or. &
                dt%l2%kind /= 2)                            error stop 33_4

        class is(base(4,*))
           error stop 102_4
        class  default
           error stop 103_4
       end select

       end subroutine
end module

  program dtParameterInquirySelectTypeParam04
  use m
  implicit none

  interface
    subroutine check1(dt)
       import
       class(base(2,:)),allocatable,intent(in)  :: dt
    end subroutine
  end interface

  class(base(kind(4),:)),pointer  :: p1 => null()
  class(base(2,:)),allocatable  :: a1
  type(child(a1%k1,2*5,kind(8),3*max(3,5))),target :: a2

  allocate(base(kind(2_2),max(3,5)) :: a1)
  call check1(a1)
  deallocate(a1)
  allocate(a1,source=a2)
  call check1(a1)

  allocate(child(a2%k2,a2%l2,a1%k1,a1%l1) :: p1)
  call check2(p1)
  deallocate(p1)

end

  subroutine check1(dt)
     use m
     class(base(2,:)),allocatable,intent(in) :: dt

     print *,"call check1"
     select type(dt)
        type is(base(2,*))
           print *,"dt is base"
           if(dt%k1 /= 2)                                   error stop 10_4
           if(dt%l1 /= 5)                                   error stop 11_4
           if(dt%k1%kind /=kind(dt%k1) .or. &
                dt%k1%kind /= 4)                            error stop 12_4
           if(dt%l1%kind /=kind(dt%l1) .or. &
                dt%l1%kind /= 4)                            error stop 13_4

        type is(child(int(2.0),*,4,*))
           print *,"dt is child"
           if(dt%k1 /= 2)                                   error stop 14_4
           if(dt%l1 /= 10)                                  error stop 15_4
           if(dt%k1%kind /=kind(dt%k1) .or. &
                dt%k1%kind /= 4)                            error stop 16_4
           if(dt%l1%kind /=kind(dt%l1) .or. &
                dt%l1%kind /= 4)                            error stop 17_4

           if(dt%k2 /= 4)                                   error stop 18_4
           if(dt%l2 /= 15)                                  error stop 19_4
           if(dt%k2%kind /=kind(dt%k2) .or.  &
                dt%k2%kind /= 4)                            error stop 20_4
           if(dt%l2%kind /=kind(dt%l2) .or. &
                dt%l2%kind /= 4)                            error stop 21_4

        class is(base(2,*))
           error stop 100_4
        class  default
           error stop 101_4
     end select

  end subroutine
