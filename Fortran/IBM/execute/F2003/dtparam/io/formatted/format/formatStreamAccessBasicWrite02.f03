!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Dec. 11 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : FORMATTED INTRINSIC IO
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  1. test WRITE statement with stream access
!*  2. derived type is polymorphic type and has nested derived type component and type bound procedure
!234567890123456789012345678901234567890123456789012345678901234567890
module m
    type dt(l)
      integer,len   :: l
      integer       :: i1(l) !l=3
      character(l)  :: c1(l)
      contains
         procedure,pass :: writeDt
    end type
    type base(l1)
      integer,len :: l1
      real        :: r1(l1)  !l1=2
      type(dt(l1+1)) :: dt1
      contains
         procedure,pass :: writeBase
    end type

    type,extends(base) :: child(l2)
      integer,len :: l2
      complex  :: x1(l2-1) !l2=3
      type(dt(l2)) :: dt2
      contains
         procedure,pass :: writeChild
    end type

    contains

      subroutine writeBase(this)
         class(base(*)),intent(in) :: this
         integer :: mypos

         select type(this)
           type is(child(*,*))
              inquire(10,pos=mypos)
!              print *,mypos
              write(10,'(f7.3,f7.3)',pos=1) this%r1
              call this%dt1%writeDt
              call this%writeChild
           class default
             stop 12
         end select
      end subroutine

      subroutine writeDt(this)
         class(DT(*)),intent(in) :: this
         integer :: mypos

         select type(this)
           type is(DT(*))
              inquire(10,pos=mypos)
!              print *,mypos
              write(10,'(3i7.4,a3,a4,a3)') this
           class default
             stop 13
         end select
      end subroutine

      subroutine writeChild(this)
         class(child(*,*)),intent(in) :: this
         integer :: mypos

         select type(this)
           type is(child(*,*))
              inquire(10,pos=mypos)
!              print *,mypos
              write(10,'(e10.3,e15.3e4,en10.3,es10.3)') this%x1
              call this%dt2%writeDt
           class default
              stop 14
         end select
      end subroutine

end module

program formatStreamAccessBasicWrite02
  use m
  implicit none

  class(base(:)),allocatable :: base1
  integer :: ios,mypos
  character(300) :: msg

  allocate(child(2,3) :: base1)

  select type(base1)
    type is(child(*,*))
      base1%r1=[1.234,-1.234]

      base1%dt1%i1=[111,-112,113]
      base1%dt1%c1=["Red","Jar","Pot"]

      base1%x1=[(0.004321,-4.321E4),(0.009876,-9.8765E-4)]

      base1%dt2%i1=[311,-312,313]
      base1%dt2%c1=["Book","Tool","Loop"]
    class default
      stop 11
  end select

  open(10,file='formatStreamAccessBasicWrite02.out',status='new',&
          form='formatted', access='stream',&
       sign='plus',decimal='comma',action='write',&
       iomsg=msg,iostat=ios)

  if(ios /= 0) then
    print *,"fail to open the file"
    print *,"iomsg=",msg
    print *,"iostat=",ios
    stop 16
  else
       call base1%writeBase

       select type(base1)
          type is(child(*,*))
             inquire(10,pos=mypos)
!             print *,mypos
             write(10,100) base1
             inquire(10,pos=mypos)
!             print *,mypos
          class default
             stop 15
       end select
  end if

100    format(2f7.3,/3i7.4,/a3,a4,a3,/e10.3,e15.3e4,/en10.3,es10.3,/3i7.4,/a3,a4,a3)
  close(10,iostat=ios)

  if(ios /= 0) then
     print *,"fail to close the file,iostat=",ios
     stop 17
  end if

end program
