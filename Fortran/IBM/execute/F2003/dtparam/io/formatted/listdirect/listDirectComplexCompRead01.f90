!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 15 2009
!*
!*  PRIMARY FUNCTIONS TESTED   : LIST-DIRECTED INTRINSIC IO
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!* 1. Test Read statement with complex as ultimate components
!* 2. Input data has different forms
!* 3. Object list is dummy argument which is passed through multiple function
!234567490123456749012345674901234567490123456749012345674901234567490
module m1
   type base(k1,l1)
     integer,kind :: k1 !k1=4
     integer,len  :: l1 !l1=3

     complex(k1) :: x1(l1)=(0.,0.)
     complex(2*k1) :: x2=(0.,0.)
   end type
end module

module m2
use m1
    type,extends(base) ::  child(k2,l2)
       integer,kind :: k2 ! k2=8
       integer,len  :: l2 ! l2=5
       complex(2*k1+k2)  :: x3(l1:l2) =(0.,0.)
    end type

    contains

    function getData1(DT,unit)
        class(child(4,*,8,*)),target,intent(inout) :: DT(:)
        class(base(4,DT%l1)),pointer :: getData1(:)
        integer,intent(in) :: unit

        allocate(getData1(2),source=getData2(DT,unit))
    end function

    function getData2(DT,unit)
        class(base(4,*)),target,intent(inout) :: DT(:)
        class(base(4,DT%l1)),pointer :: getData2(:)
        integer,intent(in) :: unit

        allocate(getData2(2),source=getData3(DT,unit))

    end function

    function getData3(DT,unit)
        class(base(4,*)),target,intent(inout) :: DT(:)
        class(base(4,DT%l1)),pointer :: getData3(:)
        integer,intent(in) :: unit
        integer :: l,u

        l=lbound(DT,1)
        u=ubound(DT,1)

        select type(DT)
           type is(child(4,*,8,*))
              read(unit,*) DT(l)%x1,DT(l)%x2 ,DT(l)%x3
              read(unit,*,decimal='point') DT(u)%x1,DT(u)%x2 ,DT(u)%x3
           class default
              stop 10
        end select

        getData3=>DT

    end function

end module

program listDirectComplexCompRead01
use m2
  integer :: i
  character(256) :: msg
  logical :: precision_x8,precision_x6,precision_x3

  implicit class(base(4,:)) (T)
  implicit type(child(4,3,8,5)) (S)

  pointer :: t1(:),t2(:)

  target :: s1(2:3)

  t1=>s1

  open(10,file='listDirectComplexCompRead01.dat',decimal='comma',&
        iostat=ios,iomsg=msg)

  if(ios <> 0) then
     print *,"fail to open the file"
     print *,"iostat=",ios
     print *,"iomsg=",msg

     stop 10
  end if

  ! following is data we want to read:
  !( -3,5E-3 ; 2,4E1 )  ( 7,6
  ! ;
  !2,3 ) ; (       15,0 ;
  !  -3,2 ) ; 1*( -3,6D-099 ; 4,2D99 ) 2*(1,1Q-108
  ! ; -0,1Q-27 ) ;  ; /
  ! ( 9.9,-9.9) , (1.2E-1,
  !2.1E-1)  3*(3.1,-4.1), ,
  ! (5.4Q-1,-3.5Q12) /

  t2(-1:)=>getData1(s1,10)

  !verify results

  select type(x=>t1)
     type is(child(4,*,8,*))
        if(.not. precision_x8(x(2)%x1(1),(-3.5E-3,2.4E1)))  stop 11
        if(.not. precision_x8(x(2)%x1(2),(7.6,2.3)))        stop 12
        if(.not. precision_x8(x(2)%x1(3),(15.0,-3.2)))      stop 13
        if(.not. precision_x6(x(2)%x2,(-3.6D-099,4.2D99)))  stop 14
        if(.not. precision_x3(x(2)%x3(3),(1.1Q-108,-0.1Q-27)))  stop 15
        if(.not. precision_x3(x(2)%x3(4),(1.1Q-108,-0.1Q-27)))  stop 16
        if(.not. precision_x3(x(2)%x3(5),(0._16,0._16)))    stop 17

        if(.not. precision_x8(x(3)%x1(1),(9.9_4,-9.9_4)))    stop 18
        if(.not. precision_x8(x(3)%x1(2),(1.2E-1,2.1E-1)))   stop 19
        if(.not. precision_x8(x(3)%x1(3),(3.1_4,-4.1_4)))    stop 20
        if(.not. precision_x6(x(3)%x2,(3.1_8,-4.1_8)))       stop 21
        if(.not. precision_x3(x(3)%x3(3),(3.1_16,-4.1_16)))  stop 22
        if(.not. precision_x3(x(3)%x3(4),(0._16,0._16)))     stop 23
        if(.not. precision_x3(x(3)%x3(5),(5.4Q-1,-3.5Q12)))  stop 24

     class default
        stop 15
  end select

  select type(x=>t2)
     type is(child(4,*,8,*))

        if(.not. precision_x8(x(-1)%x1(1),(-3.5E-3,2.4E1)))  stop 25
        if(.not. precision_x8(x(-1)%x1(2),(7.6,2.3)))        stop 26
        if(.not. precision_x8(x(-1)%x1(3),(15.0,-3.2)))      stop 27
        if(.not. precision_x6(x(-1)%x2,(-3.6D-099,4.2D99)))  stop 28
        if(.not. precision_x3(x(-1)%x3(3),(1.1Q-108,-0.1Q-27)))  stop 29
        if(.not. precision_x3(x(-1)%x3(4),(1.1Q-108,-0.1Q-27)))  stop 30
        if(.not. precision_x3(x(-1)%x3(5),(0._16,0._16)))    stop 31

        if(.not. precision_x8(x(0)%x1(1),(9.9_4,-9.9_4)))    stop 32
        if(.not. precision_x8(x(0)%x1(2),(1.2E-1,2.1E-1)))   stop 33
        if(.not. precision_x8(x(0)%x1(3),(3.1_4,-4.1_4)))    stop 34
        if(.not. precision_x6(x(0)%x2,(3.1_8,-4.1_8)))       stop 35
        if(.not. precision_x3(x(0)%x3(3),(3.1_16,-4.1_16)))  stop 36
        if(.not. precision_x3(x(0)%x3(4),(0._16,0._16)))     stop 37
        if(.not. precision_x3(x(0)%x3(5),(5.4Q-1,-3.5Q12)))  stop 38
     class default
        stop 39
  end select

  close(10)

end program
