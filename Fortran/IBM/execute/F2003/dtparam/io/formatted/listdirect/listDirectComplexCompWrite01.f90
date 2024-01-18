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
!* 1. Test Write & Read statement
!* 2. Derived type has complex ultimate components
!* 3. Derived type is polymorphic type and has type-bound procedure and generic binding
!234567890123456789012345678901234567890123456789012345678901234567890
module m1
   type base(k1,l1)
      integer,kind :: k1
      integer,len  :: l1 ! l1=1

      complex(k1) :: x1(l1)
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

      complex(k1+k2) :: x2(l1:l2)
      contains
         procedure ::  writeDT=>writeChild
         generic :: write=>writeDT
  end type

  type,extends(child) :: gen3(k3,l3)
       integer,kind :: k3
       integer,len  :: l3 ! l3=4

       complex(k1+k2+k3) :: x3(l1+l2:l3)

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
             x(2)%x1=[(-1.23E-03,1.23E3)]
             x(2)%x2=[(0.789D9,-78.9D-9),(15.2D-100,-15.2D100)]
             x(2)%x3=[(-1.23Q+208,1.23Q-208),(1.15Q2,-1.15Q-2)]
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
              ! write the slash separator to ignore remainning items when reading input later
              write(unit,*,decimal='COMMA') dt,"/"
           class default
             stop 14
        end select

    end subroutine
end module


program listDirectComplexCompWrite01
  use m2
  implicit none

  integer :: ios=0,unit=1024,i
  character(256) :: msg
  logical :: precision_x8,precision_x6,precision_x3

  class(base(4,1)),allocatable,target :: poly1(:),poly2(:)

  class(*),pointer :: upoly1(:)=>null()

  call associate(upoly1,poly1)

  open(unit=unit,file='listDirectComplexCompWrite01.out',form='formatted', &
      access='sequential',status='replace', &
      sign='plus',decimal='comma',iostat=ios,iomsg=msg)

  if( ios <> 0) then
     write(unit,*) "fail to open the file"
     write(unit,*) "iostat=",ios
     write(unit,*) "iomsg=",msg
     stop 11
  end if

  do i=lbound(poly1,1),ubound(poly1,1)
     ! will call writeGen3
     call poly1(i)%write(unit)
     associate(x=>poly1(i))
        select type(x)
           type is(gen3(4,*,4,*,8,*))
             ! will call writeChild
             call x%child%write(unit)
               associate(y=>x%child)
                   !will call writeBase
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
        poly2(-1)%x1=(1.0_4,-1.0_4)
        poly2(-1)%x2=(1.1_8,-1.1_8)
        poly2(-1)%x3=(1.2_16,-1.2_16)
        ! value of poly2(-2) comes from read, rest of items after slash will be ignored when read into array poly2
        read(unit,*) poly2
    class default
       stop 17
  end select

  !verify results
  do i=lbound(upoly1,1),ubound(upoly1,1)
     select type(x=>upoly1(i))
        type is(gen3(4,*,4,*,8,*))
           if(.not. precision_x8(x%x1,(-1.23E-03,1.23E3)))        error stop 18
           if(.not. precision_x6(x%x2(1),(0.789D9,-78.9D-9)))     error stop 19
           if(.not. precision_x6(x%x2(2),(15.2D-100,-15.2D100)))  error stop 20
           if(.not. precision_x3(x%x3(3),(-1.23Q+208,1.23Q-208))) error stop 21
           if(.not. precision_x3(x%x3(4),(1.15Q2,-1.15Q-2)))      error stop 22
        class default
           stop 23
     end select
  end do

  select type(poly2)
    type is(gen3(4,*,4,*,8,*))
       if(.not. precision_x8(poly2(-2)%x1,(-1.23E-03,1.23E3)))        error stop 24
       if(.not. precision_x6(poly2(-2)%x2(1),(0.789D9,-78.9D-9)))     error stop 25
       if(.not. precision_x6(poly2(-2)%x2(2),(15.2D-100,-15.2D100)))  error stop 26
       if(.not. precision_x3(poly2(-2)%x3(3),(-1.23Q+208,1.23Q-208))) error stop 27
       if(.not. precision_x3(poly2(-2)%x3(4),(1.15Q2,-1.15Q-2)))      error stop 28

       if(.not. precision_x8(poly2(-1)%x1,(1.0_4,-1.0_4)))            error stop 29
       if(.not. precision_x6(poly2(-1)%x2(1),(1.1_8,-1.1_8)))         error stop 30
       if(.not. precision_x6(poly2(-1)%x2(2),(1.1_8,-1.1_8)))         error stop 31
       if(.not. precision_x3(poly2(-1)%x3(3),(1.2_16,-1.2_16)))       error stop 32
       if(.not. precision_x3(poly2(-1)%x3(4),(1.2_16,-1.2_16)))       error stop 33
    class default
       stop 34
  end select

  close(unit,status='keep')

end program
