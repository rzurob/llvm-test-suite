!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 16 2009
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type A(l)
     integer,len  :: l
  end type
  contains
    function fun(dt)
      type(A(*)) :: dt
     ! type(A(dt%l)) :: fun
      implicit type(A(dt%l))(f)

      fun=dt
    end function
end module

program d361081
  use m
  implicit type(A(:)) (f)
  allocatable :: f1

  f1= fun(A(3)())

  if(f1%l /= 3)  error stop 10

end program

