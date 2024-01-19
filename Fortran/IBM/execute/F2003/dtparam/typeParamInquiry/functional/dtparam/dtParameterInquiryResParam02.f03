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
!* 3. FUNCTION RESULT IS CHARACTER
!234567890123456789012345678901234567890123456789012345678901234567890
module m
     type base(l1,l2)
        integer(2),len :: l1
        integer(4),len :: l2
     end type
end module

  program dtParameterInquiryResParam02
  use m
  implicit none

  class(*),pointer :: b1
  type(base(:,:)), allocatable :: b2
  integer :: i=0

  allocate(base(4,6) :: b1)
  select type(b1)
     type is(base(*,*))
       if(lbound(setChar3(b1%l1,b1%l2),1) /= 1 .or.    &
          ubound(setChar3(b1%l1,b1%l2),1) /= 3 )          error stop 10_4
       if(any(setChar3(b1%l1,b1%l2) /=  &
          (/(char(i),i=b1%l1,b1%l2)/)) )                  error stop 11_4

       if(len(setChar1(b1%l1,b1%l2)) /= (b1%l1+b1%l2) )   error stop 12_4
       if(size(setChar1(b1%l1,b1%l2)) /= (b1%l1*b1%l2))   error stop 13_4
       if(any(setChar1(b1%l1,b1%l2) /= 'a' ))             error stop 14_4

  end select

  allocate(base(2,3) :: b2)

       if(lbound(setChar3(b2%l1,b2%l2),1) /= 1 .or.    &
          ubound(setChar3(b2%l1,b2%l2),1) /= 2 )          error stop 15_4
       if(any(setChar3(b2%l1,b2%l2) /=  &
          (/(char(i),i=b2%l1,b2%l2)/)) )                  error stop 16_4

       if(len(setChar1(b2%l1,b2%l2)) /= (b2%l1+b2%l2) )   error stop 17_4
       if(size(setChar1(b2%l1,b2%l2)) /= b2%l1*b2%l2)     error stop 18_4
       if(any(setChar1(b2%l1,b2%l2) /= 'a' ))             error stop 19_4

       if(len(setChar2(b2)) /= (b2%l1+b2%l2) )            error stop 20_4
       if(size(setChar2(b2)) /= 6)                        error stop 21_4

       associate(x=>setChar2(b2))
          do i=1,size(x)
             if(x(i) /= char(i))                          error stop 22_4
          end do
       end associate

  contains


  function setChar1(l1,l2)
     integer(2),intent(in) :: l1
     integer(4),intent(in) :: l2
     character(l1+l2) :: setChar1(l1,l2)

     setChar1='a'
  end function

  function setChar2(dt)
    type(base(*,*)) :: dt
    character(len=dt%l1+dt%l2) :: setChar2(dt%l1%kind+dt%l2%kind)

    do i=1,dt%l1%kind+dt%l2%kind
       setChar2(i)=char(i)
    end do
  end function

  function setChar3(lbd,ubd)
     integer(2),intent(in) :: lbd
     integer,intent(in) :: ubd
     character,dimension(lbd:ubd) :: setChar3

     setChar3=(/(char(i),i=lbd,ubd)/)
  end function

end

