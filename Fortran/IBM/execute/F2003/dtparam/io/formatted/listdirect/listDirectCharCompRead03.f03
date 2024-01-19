!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 13 2009
!*
!*  PRIMARY FUNCTIONS TESTED   : LIST-DIRECTED INTRINSIC IO
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!* 1. Test READ statement in type bound procedure, also use generic binding
!* 2. Test following form:
!*     c, r*c,r*
!* 3. Test null value:
!*    1) r* form
!*    2) no character between consecutive value separators, or
!*    3) no characters before the first value separator in the first record read by each execution of a list-directed input statement
!234567890123456789012345678901234567890123456789012345678901234567890
module m1
  type A(k1,l1)
     integer,kind  :: k1 ! k1=2
     integer,len   :: l1 ! l1=4
     character(k1) :: c1(l1)
     contains
       procedure :: readA
  end type

  contains

    subroutine readA(at,unit)
      implicit class(A(2,*)) (a)
      integer,intent(in) :: unit
      intent(inout) :: at

      select type(x=>at)
        type is(A(2,*))
            read(unit,fmt=*) x
        class default
           stop 13
      end select
    end subroutine

end module

module m2
use m1
  type base(k2,l2)
     integer,kind :: k2 ! k2=4
     integer,len  :: l2 ! l2=3

     character(3) :: c2(4)
     type(A(k2/2,l2+1)) :: a1
     contains
         procedure :: readDT=>readBase
         generic   :: read=>readDT

  end type

  contains

    subroutine readBase(tt,unit)
       implicit class(base(4,*)) (t)
       integer,intent(in) :: unit

       intent(inout) :: tt

       select type(x=>tt)
          type is(base(4,*))

             read(unit,fmt=*) x%c2
             call x%a1%readA(unit)
          class default
             stop 12
       end select
    end subroutine

end module

module m3
use m2
  type,extends(base) :: child(k3,l3)
     integer,kind :: k3 ! k3=8
     integer,len  :: l3 ! l3=5

     character(k3) :: c3(k3)
     type(A(k3/4,l3-1)) :: a2

     contains
        procedure :: readDT=>readChild
        generic :: read=>readDT
  end type

  contains

    subroutine readChild(tt,unit)
       implicit class(child(4,*,8,*))(T)
       integer,intent(in) :: unit

       intent(inout) :: tt

       select type(x=>tt)
          type is(child(4,*,8,*))

             call x%base%read(unit)
             read(unit,fmt=*)  x%c3
             call x%a2%readA(unit)

          class default
            stop 11
       end select
    end subroutine

end module

program listDirectCharCompRead03
  use m3

  integer :: ios,i
  character(256) :: msg

  implicit class(child(4,:,8,:)) (c)

  allocatable :: ct

  allocate(child(4,3,8,5) :: ct)

  ! initialize ct
  select type(ct)
      type is(child(4,*,8,*))
          ct%c2="red"
          ct%a1%c1="hi"
          ct%c3="12345678"
          ct%a2%c1="to"
      class default
          stop 9
  end select

  open(10,file='listDirectCharCompRead03.dat', delim='quote',&
      status='old',iostat=ios,iomsg=msg)

  if(ios <> 0) then
     print *,"fail to open the file"
     print *,"iostat=",ios
     print *,"iomsg=",msg
     stop 10
  end if

  ! following is the input value:

  !,   ,1*old ,new
  !2*HE ,LE 1*FT
  !XLF 1* , 2*TEST''TEAM ,1*.TRUE. ,,"(5.,6.)" 1*
  !1*ab,, cd,1*1,/

  call ct%read(10)

  ! output results for verification
  select type(ct)
     type is(child(4,*,8,*))
         do i=1,4
            write(*,*) ct%c2(i)
         end do

         do i=1,4
            write(*,*) ct%a1%c1(i)
         end do

         do i=1,8
            write(*,*) ct%c3(i)
         end do

         do i=1,4
           write(*,*) ct%a2%c1(i)
         end do
     class default
         stop 14
  end select

  close(10)

end program

