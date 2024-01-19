!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : July 27 2008
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
!* 3. FUNCTION RESULT AS LENGTH TYPE PARAMETER
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type base(l1)
     integer(4),len :: l1=4
   end type

   type,extends(base) :: child(l2)
     integer(4),len :: l2=6
   end type
end module

  program dtParameterInquiryResParam03
  use m
  implicit none

  interface
     function getlen1(dt)
        import
        class(base(*)) :: dt
        integer(8) :: getlen1
     end function

  end interface

  type(child) :: c1
  type(base)  :: b1


  class(base(:)),allocatable :: b2
  class(*),pointer :: p1

  type(child(:,:)),allocatable :: c2
  class(base(:)),pointer  :: p2


  allocate(base(l1=getlen1(b1)) :: b2)
  select type(b2)
     type is(base(*))
        if(b2%l1 /= 4)                                  error stop 10_4
     class default
        error stop 100_4
  end select
  deallocate(b2)

  allocate(child(l1=getlen1(c1),l2=getlen1(b1)) :: b2)
  select type(b2)
     type is(child(*,*))
        if(b2%l1 /= c1%l1+c1%l2)                        error stop 11_4
        if(b2%l2 /= b1%l1)                              error stop 12_4
     class default
        error stop 101_4
  end select

  allocate(child(l1=getlen1(c1)-getlen1(b1),l2=getlen1(c1)+getlen1(b1)) ::p1)
  select type(x=>p1)
      type is(child(*,*))
      if(x%l1 /= 6)                                     error stop 13_4
      if(x%l2 /= 14)                                    error stop 14_4
      class default
        error stop 102_4
  end select
  deallocate(p1)

  allocate(base(c1%l1+getlen2()) :: p1)
  select type(p1)
    type is(base(*))
        if(p1%l1 /= 18)                                 error stop 15_4
    class default
        error stop 103_4
  end select
  deallocate(p1)

  contains

     integer function getlen2()
        getlen2=b1%l1 + c1%l1 + c1%l2
     end function
end

     function getlen1(dt)
        use m,only : base,child
        class(base(*)) :: dt
        integer(8) :: getlen1

        select type(dt)
          type is(child(*,*))
             getlen1=dt%l1+dt%l2
          type is(base(*))
             getlen1=dt%l1
          class default
             error stop 104_4
        end select
     end function

