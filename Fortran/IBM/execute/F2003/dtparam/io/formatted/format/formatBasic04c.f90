!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Dec. 7 2008
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  1. test READ statement
!*  2  derived type is polymorphic which has logical ultimate component
!*  3. use lw,gw.d,gw.dEe edit descriptor
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type base(k1)
      integer,kind :: k1
      logical(k1)  :: log1(k1)
      logical(k1)  :: log2(k1-1:k1+1)
   end type
   type,extends(base) :: child(l1)
      integer,len  :: l1
      logical      :: log3(k1:l1)
   end type

   contains

   subroutine readdata(arg)
     class(base(2)),intent(inout) :: arg(:)

     select type(arg)
        type is(child(2,*))
          read(10,100) arg
100 format(2l5/3g5.2/2g5.2E1)
        class default
          error stop 100_4
     end select
   end subroutine

end module

program formatBasic04c
  use m
  implicit none

  class(base(2)),target,allocatable :: tbase(:)
  class(base(2)),pointer  :: pbase(:)=>null()
  integer :: ios
  character(256) :: msg

  allocate(child(2,3) :: tbase(3))

  pbase(0:)=>tbase(3:1:-1)

  open(10 ,file="formatBasic04c.in",iostat=ios)

  if(ios /= 0) then
     print *,"fail to open the file"
     print *,"iostat=",ios
     print *,"iomsg=",msg
     error stop 103_4
  else
      call readdata(tbase)

      select type(pbase)
        type is(child(2,*))
           write(*,*) "first read"
           write(*,'(2l5/3g5.2/2g5.2E2)' ) pbase
        class default
           error stop 101_4
      end select

      rewind(10)

      call readdata(pbase)

      select type(pbase)
        type is(child(2,*))
           write(*,*) "second read"
           write(*,'(2l5/3g5.2/2g5.2E2)' ) pbase
        class default
           error stop 102_4
      end select

  end if

end program
