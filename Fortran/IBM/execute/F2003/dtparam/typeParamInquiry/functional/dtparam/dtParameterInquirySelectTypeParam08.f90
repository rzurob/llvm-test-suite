!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParameterInquirySelectTypeParam08.f
!*
!*  DATE                       : July 26 2008
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
!* 3. UNLIMITED POLYMORPHIC
!234567890123456789012345678901234567890123456789012345678901234567890
module m
       type base(l1)
          integer(2),len :: l1
       end type

       type,extends(base) :: child(l2)
          integer(8),len :: l2
       end type

end module

  program dtParameterInquirySelectTypeParam08
  use m
  implicit none

  integer(kind(8)),target :: i=2
  class(*),pointer :: p1=>null()
  p1=>i

  select type(x=>p1)
     type is(integer(kind(8)))
        print *,"type is integer"
        print *,x%kind,kind(x)
     class default
       error stop 100_4
  end select
  select type(x=>gettype(p1))
      type is(integer(kind(8)))
        print *,"function return type is integer"
        print *,x%kind,kind(x)
      class default
        error stop 101_4
  end select

  allocate(base(4) :: p1)

  select type(p1)
     type is(base(*))
        print *,"type is base"
        print *,p1%l1,p1%l1%kind,kind(p1%l1)
     class default
        error stop 102_4
  end select

  select type(x=>gettype(p1))
     type is(base(*))
        print *,"function return type is base"
        print *,x%l1,x%l1%kind,kind(x%l1)
     class default
        error stop 103_4
  end select

  deallocate(p1)
  allocate(child(selected_int_kind(3),int(-4.4)) :: p1)

  select type(p1)
     type is(child(*,*))
        print *,"type is child"
        print *,p1%l1,p1%l1%kind,kind(p1%l1)
        print *,p1%l2,p1%l2%kind,kind(p1%l2)
     class default
       error stop 104_4
  end select

  select type(x=>gettype(p1))
     type is(child(*,*))
        print *,"function return type is child"
        print *,x%l1,x%l1%kind,kind(x%l1)
        print *,x%l2,x%l2%kind,kind(x%l2)
     class default
       error stop 105_4
  end select


  contains
    class(*) function  gettype(t)
       class(*),intent(in) :: t
       pointer :: gettype

       print *,"in gettype"
       select type(t)
          type is(integer(kind(8)))
            print *,"it is integer"
            print *,t%kind,kind(t)
            allocate(gettype,source=t)
          type is(base(*))
            print *,"it is base"
            print *,t%l1,t%l1%kind,kind(t%l1)
            allocate(gettype,source=t)
          type is(child(*,*))
            print *,"it is child"
            print *,t%base%l1,t%base%l1%kind,kind(t%base%l1)
            print *,t%l2,t%l2%kind,kind(t%l2)
            allocate(gettype,source=t)
          class default
           error stop 106_4
       end select
       print *,"exit gettype"
    end function
end

