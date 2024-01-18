!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : listDirectIntCompRead02.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : Jan. 14 2009 
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : LIST-DIRECTED INTRINSIC IO 
!*
!*  SECONDARY FUNCTIONS TESTED :  
!*
!*  REFERENCE                  : 
!*
!*  DRIVER STANZA              : xlf2003
!*
!*
!*  DESCRIPTION
!* 1. Test Read statement with integer as components
!* 2. Execute Read statement with dummy argument as input object, and dummy argument is passed through multiple subroutines  
!234567890123456789012345678901234567890123456789012345678901234567890
module m1
   type base(k1,l1)
      integer,kind :: k1
      integer,len  :: l1
      
      integer(k1) :: i1(l1)
   end type
end module

module m2
use m1
   type,extends(base) :: child(k2,l2)
      integer,kind :: k2
      integer,len  :: l2
      integer(k1+k2) :: i2(l1:l2+1)
   end type

   contains

     subroutine sub1(arg)
         class(base(4,*)),intent(inout) :: arg(2:)
        
         call sub2(arg(ubound(arg,1):lbound(arg,1):-1) )
     end subroutine

     subroutine sub2(arg)
         type(base(4,*)),intent(inout) :: arg(:)
         call sub3(arg)
     end subroutine

     subroutine sub3(arg)
         class(base(4,*)),intent(inout) :: arg(:)
         select type(arg)
           type is(base(4,*))
              read(10,*,decimal='point')    arg 
           class default
              stop 11 
         end select
     end subroutine

     subroutine sub4(arg)
         class(child(4,*,4,*)),intent(inout) :: arg(:)

         call sub5(arg( ubound(arg,1) : lbound(arg,1) : -1) )
     end subroutine

     subroutine sub5(arg)
         type(child(4,*,4,*)),intent(inout) :: arg(:)
        
         do i=lbound(arg,1),ubound(arg,1) 
            read(10,*,decimal='comma') arg(i)%i2
         end do
     end subroutine
   
end module

program listDirectIntCompRead02
  use m2
  implicit none

  integer :: ios
  character(256) :: msg

  class(base(4,3)),allocatable :: obj(:)

  allocate(obj(2),source=child(4,3,4,5)(i1=-99,i2=-99))

  open(10,file="listDirectIntCompRead02.dat",iostat=ios,iomsg=msg)

  if(ios <> 0) then
    print *,"fail to open the file"
    print *,"iostat=",ios
    print *,"iomsg=",msg
    stop 10
  end if

  ! following is the input we want to read, there are extra value in the input 

  !-20 , 1*-30 2*+21, 1* , - 7
  ! ;-52 +04 ; ;+2*-1;
  ! 33 1*-10 ; ; 100*44

  call sub1(obj)

  select type(obj)
    type is(child(4,*,4,*))
       call sub4(obj)
    class default
        stop 12
  end select

  ! output value for verification
  select type(obj)
     type is(child(4,*,4,*))
        write(*,*,sign='plus') obj
     class default 
        stop 13
  end select

  close(10)
 
end program
