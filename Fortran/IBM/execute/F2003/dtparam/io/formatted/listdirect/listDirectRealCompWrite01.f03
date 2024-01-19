!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 9 2009
!*
!*  PRIMARY FUNCTIONS TESTED   : LIST-DIRECTED INTRINSIC IO
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  1. Test Write and Read when Derived type has real ultimate components
!*  2. Derived type is polymorphic type and has type bound procedure and generic binding
!234567890123456789012345678901234567890123456789012345678901234567890
module m1
   type base(k1,l1)
      integer,kind :: k1
      integer,len  :: l1 ! l1=1

      real(k1) :: r1(l1)
      contains
         procedure ::  writeDT=>writeBase
         generic :: write=>writeDT
   end type

   contains

    subroutine writeBase(dt,unit)
        class(base(4,*)),intent(in) :: dt
        integer,intent(in) :: unit

        print *," in writeBase"
        select type(dt)
           type is(base(4,*))
              write(unit,fmt=*,decimal='POINT') dt
           class default
              stop 12
        end select
    end subroutine

end module

module m2
  use m1

  type,extends(base) :: child(k2,l2)
      integer,kind  :: k2
      integer,len   :: l2 ! l2=2

      real(k1+k2) :: r2(l1:l2)
      contains
         procedure ::  writeDT=>writeChild
         generic :: write=>writeDT
  end type

  type,extends(child) :: gen3(k3,l3)
       integer,kind :: k3
       integer,len  :: l3 ! l3=4

       real(k1+k2+k3) :: r3(l1+l2:l3)

       contains
         procedure :: writeDT=>writeGen3
         generic   :: write=>writeDT
  end type

  contains

    subroutine associate(ptr,target)
       class(base(4,*)),allocatable,target,intent(inout) :: target(:)
       class(*),pointer,intent(inout) :: ptr(:)

       allocate(gen3(4,*,4,2,8,4) :: target(2:2))

       select type(x=>target)
          type is(gen3(4,*,4,*,8,*))
             x(2)%r1=[-1.23E-03]
             x(2)%r2=[0.789D9,-78.9D-9]
             x(2)%r3=[-1.23Q+208,1.23Q-208]
          class default
             stop 10
       end select

       ptr(-2:)=>target

    end subroutine

    subroutine writeChild(dt,unit)
        class(child(4,*,4,*)),intent(in) :: dt
        integer,intent(in) :: unit

        print *," in writeChild"
        select type(dt)
           type is(child(4,*,4,*))
              write(unit,*) dt
           class default
             stop 13
        end select
    end subroutine

    subroutine writeGen3(dt,unit)
        class(gen3(4,*,4,*,8,*)),intent(in) :: dt
        integer,intent(in) :: unit

        print *," in writeGen3"
        select type(dt)
           type is(gen3(4,*,4,*,8,*))
              ! write the slash separator to ignore remainning items when reading inputs later
              write(unit,*,decimal='COMMA') dt,"/"
           class default
             stop 14
        end select

    end subroutine
end module


program listDirectRealCompWrite01
  use m2
  implicit none

  integer :: ios=0,unit=1024,i
  character(256) :: msg
  logical :: precision_r4,precision_r8,precision_r16

  class(base(4,1)),allocatable,target :: poly1(:),poly2(:)

  class(*),pointer :: upoly1(:)=>null()

  call associate(upoly1,poly1)

  open(unit=unit,file='listDirectRealCompWrite01.out',form='formatted', &
      access='sequential',status='replace', &
      sign='plus',decimal='comma',iostat=ios,iomsg=msg)

  if( ios <> 0) then
     write(unit,*) "fail to open the file"
     write(unit,*) "iostat=",ios
     write(unit,*) "iomsg=",msg
     stop 11
  end if

  do i=lbound(poly1,1),ubound(poly1,1)
     call poly1(i)%write(unit)
     associate(x=>poly1(i))
        select type(x)
           type is(gen3(4,*,4,*,8,*))
             call x%child%write(unit)
               associate(y=>x%child)
                   call y%base%write(unit)
               end associate
           class default
              stop 16
        end select
     end associate
  end do

  ! rewind back for read
  rewind unit

  allocate(gen3(4,1,4,2,8,4) :: poly2(-2:-1))

  select type(poly2)
    type is(gen3(4,*,4,*,8,*))
        ! assign value to poly2(-1)
        poly2(-1)%r1=1.0_4
        poly2(-1)%r2=1.1_8
        poly2(-1)%r3=1.2_16
        ! value of poly2(-2) comes from read, rest of items after slash will be ignored when read into array poly2
        read(unit,*) poly2
    class default
       stop 17
  end select

  !verify results
  do i=lbound(upoly1,1),ubound(upoly1,1)
     select type(x=>upoly1(i))
        type is(gen3(4,*,4,*,8,*))
           if(.not. precision_r4(x%r1,-1.23E-03))        error stop 18
           if(.not. precision_r8(x%r2(1),0.789D9))       error stop 19
           if(.not. precision_r8(x%r2(2),-78.9D-9))      error stop 20
           if(.not. precision_r16(x%r3(3),-1.23Q+208))   error stop 21
           if(.not. precision_r16(x%r3(4),1.23Q-208))    error stop 22
        class default
           stop 23
     end select
  end do

  select type(poly2)
    type is(gen3(4,*,4,*,8,*))
       if(.not. precision_r4(poly2(-2)%r1,-1.23E-03))           error stop 24
       if(.not. precision_r8(poly2(-2)%r2(1),0.789D9))          error stop 25
       if(.not. precision_r8(poly2(-2)%r2(2),-78.9D-9))         error stop 26
       if(.not. precision_r16(poly2(-2)%r3(3),-1.23Q+208))      error stop 27
       if(.not. precision_r16(poly2(-2)%r3(4),1.23Q-208))       error stop 28

       if(.not. precision_r4(poly2(-1)%r1,1.0_4))               error stop 29
       if(.not. precision_r8(poly2(-1)%r2(1),1.1_8))            error stop 30
       if(.not. precision_r8(poly2(-1)%r2(2),1.1_8))            error stop 31
       if(.not. precision_r16(poly2(-1)%r3(3),1.2_16))          error stop 32
       if(.not. precision_r16(poly2(-1)%r3(4),1.2_16))          error stop 33

    class default
       stop 34
  end select

  close(unit,status='keep')

end program
