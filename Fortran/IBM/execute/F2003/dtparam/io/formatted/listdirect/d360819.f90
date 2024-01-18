!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 9 2008
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type dtp(l1)
       integer,len :: l1
       integer  :: i
   end type

  contains
   function Fun(dt)
     type(dtp(3)),intent(in) :: dt
     type(dtp(:)),allocatable :: Fun

     allocate(Fun,source=dt)
   end function
end module

program d360819
  use m
  implicit none

  type(dtp(3)) :: obj=dtp(3)(99)
  print *,"|",Fun(obj),"|"
  associate(x=>Fun(obj))
    print *,"|",x%i,"|",x,"|"
  end associate
end program
