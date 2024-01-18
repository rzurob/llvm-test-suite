!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : formatStreamAccessBasicRead02.f
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
!*  1. test READ statement with stream access
!*  2. read in type bound procedure
!*  3. derived type is polymorphic type
!234567890123456789012345678901234567890123456789012345678901234567890
module m
    type dt(l)
      integer,len   :: l
      integer       :: i1(l) !l=3
      character(l)  :: c1(l)
      contains
         procedure,pass :: readDt
    end type
    type base(l1)
      integer,len :: l1
      real        :: r1(l1)  !l1=2
      type(dt(l1+1)) :: dt1
      contains
         procedure,pass :: readBase
    end type

    type,extends(base) :: child(l2)
      integer,len :: l2
      complex  :: x1(l2-1) !l2=3
      type(dt(l2)) :: dt2
      contains
         procedure,pass :: readChild
    end type

    integer,parameter :: DT1=1,DT2=2

    contains

      subroutine readBase(this)
         class(base(*)),intent(inout) :: this
         integer :: mypos

         select type(this)
           type is(child(*,*))
              inquire(10,pos=mypos)
              read(10,'(f5.3,f5.3)',pos=mypos) this%r1
!              inquire(10,pos=mypos)
              call this%dt1%readDt(DT1)
              call this%readChild
           class default
             stop 12
         end select
      end subroutine

      subroutine readDt(this,dt)
         class(DT(*)),intent(inout) :: this
         integer :: dt

         select type(this)
           type is(DT(*))
              if(dt .eq. DT1) then
                  read(10,'(3a3)',pos=12) this%c1
                  read(10,'(bz,sp,i3,i3,i4)',pos=23) this%i1
              else
                  read(10,'(a2,a3,a4)',pos=14) this%c1
                  read(10,'(bn,3i2)',pos=1) this%i1
              end if
           class default
             stop 13
         end select
      end subroutine

      subroutine readChild(this)
         class(child(*,*)),intent(inout) :: this

         select type(this)
           type is(child(*,*))
              read(10,'(e7.1,f4.2)',pos=35)  this%x1(1)
              read(10,'(f4.2,f7.2)',pos=1)  this%x1(2)
              call this%dt2%readDt(DT2)
           class default
              stop 14
         end select
      end subroutine

end module

program formatStreamAccessBasicRead02
  use m
  implicit none

  class(base(:)),allocatable :: base1
  integer :: ios
  character(300) :: msg

  allocate(child(2,3) :: base1)

  open(10,file='formatStreamAccessBasicRead02.in',status='old',&
          form='formatted', access='stream',&
       action='read',iomsg=msg,iostat=ios)

  if(ios /= 0) then
    print *,"fail to open the file"
    print *,"iomsg=",msg
    print *,"iostat=",ios
    stop 16
  else
       call base1%readBase
       select type(base1)
          type is(child(*,*))
            write(*,'("base1%r1=[",f6.2,",",f6.2,")]")' )  base1%r1

             write(*,'("base1%x1=[(",sp,e8.1,",",f5.2,")",",(",f6.2,",",f7.2,")]")' )  base1%x1
             write(*,'("base1%dt1%i1=[",ss,i4,",",i4,",",i4,"]")' ) base1%dt1%i1
             write(*,'("base1%dt1%c1=[",a3,",",a3,",",a3,"]")' ) base1%dt1%c1
             write(*,'("base1%dt2%i1=[",sp,i4,",",i4,",",i4,"]")' ) base1%dt2%i1
             write(*,'("base1%dt2%c1=[",a3,",",a3,",",a3,"]")' ) base1%dt2%c1

          class default
              stop 15
       end select
  end if


  close(10,iostat=ios)

  if(ios /= 0) then
     print *,"fail to close the file,iostat=",ios
     stop 17
  end if

end program
