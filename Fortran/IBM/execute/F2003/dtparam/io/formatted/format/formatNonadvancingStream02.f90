!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Dec. 19 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : FORMATTED INTRINSIC IO
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!* 1. test READ & WRITE with nonadvancing IO and stream access method
!* 2. derived type is polymorphic which also has type-bound procedure, and read,write inside procedure
!234567890123456789012345678901234567890123456789012345678901234567890
module m
    type inner1(l1)
        integer,len :: l1 ! l1=3
        character(l1) :: c1(l1)
    end type

    type inner2(l2)
        integer,len   :: l2 ! l2=5
        integer       :: i1(l2)
    end type

    type base(l3)
        integer,len   :: l3 ! l3=2
        logical       :: g1(l3)
        type(inner1(l3+1)) :: in1
        contains
           procedure,pass :: writebase
           procedure,pass :: readbase
    end type

    type,extends(base):: child(l4)
        integer,len   ::  l4  ! l4=3
        type(inner2(l3+l4)) :: in2
        contains
          procedure,pass  :: writechild
          procedure,pass  :: readchild
    end type

    contains

         subroutine writebase(this)
            class(base(*)),intent(in) :: this

            select type(this)
               type is(base(*))
                  write(10,'(l2,",",l2)',advance='yes') this%g1
                  write(10,'(3(a3,:,","))',advance='yes') this%in1%c1
               class default
                  stop 12
            end select
         end subroutine

         subroutine readbase(this)
            class(base(*)),intent(inout) :: this
            integer :: pos,eor,count

            select type(this)
               type is(base(*))

                  ! read the first record
                  read(10,'(t5,l1)',advance='no',eor=100,size=count) this%g1(1)
                  if(count /= 1)       error stop 17

                  inquire(10,pos=pos)
                  if(pos /= 6)          error stop 18

                  ! back to the beginning of first record
                  rewind 10

                  read(10,'(l2,tr4)',advance='no',size=count) this%g1(2)
                  if(count /= 2)        error stop 19

                  inquire(10,pos=pos)
                  if(pos /= 7)          error stop 20

                  ! read  in second record
                  read(10,'(a3,tr5,a3)',advance='no', &
                          size=count,pos=7)                 this%in1%c1(3:2:-1)
                  if(count /= 6)       error stop 21

                  inquire(10,pos=pos)
                  if(pos /= 18)        error stop 22

                  ! return to beginning of second record

                  backspace 10

                  read(10,'(tr4,a3)',advance='no',size=count) this%in1%c1(1)
                  if(count /= 3)        error stop 23

                  inquire(10,pos=pos)
                  if(pos /= 14)         error stop 24

                class default
                  stop 15
            end select

           return

100        print *,"end of record reached,iostat=",ios
           stop 16

         end subroutine

         subroutine writechild(this)
            class(child(*,*)),intent(inout) :: this

            select type(this)
                type is(child(*,*))
                   call this%base%writebase
                   write(10,'(sp,5(i3,:,","))',advance='yes') this%in2%i1
                class default
                   stop 13
            end select
         end subroutine

         subroutine readchild(this)
            class(child(*,*)),intent(inout) :: this
            integer :: size,count,pos

            select type(this)
               type is(child(*,*))
                  call this%base%readbase

                  ! read the third record
                  read(10,'(tr12,i3,tr1,i3)',advance='no', &
                           size=count,pos=19)               this%in2%i1(2:1:-1)

                  if(count /= 6)        error stop 25

                  inquire(10,pos=pos)
                  if(pos /= 38)         error stop 26

                  ! point to the beginning of the third record
                  backspace 10
                  read(10,'(i3,tr1,i3,tr1,i3)',advance='no', &
                          size=count,pos=19)   this%in2%i1(5:3:-1)

                  if(count /= 9)       error stop 27

                  inquire(10,pos=pos)
                  if(pos /= 30)        error stop 28

               class default
                 stop 14
            end select
         end subroutine

end module

program formatNonadvancingStream02
  use m
  implicit none

  integer :: ios
  character(300) :: msg

  class(base(2)),allocatable :: poly1

  allocate(poly1,source= &
           child(2,3)(g1=[.true.,.false.], &
                     in1=inner1(3)(["IBM","XLF","AIX"]),&
                     in2=inner2(5)([11,-12,13,-14,15]) ) )

  open(10,file='formatNonadvancingStream02.dat',form='formatted',&
          status='replace',action='readwrite',access='stream',&
          blank='zero',iostat=ios,iomsg=msg)

  if(ios <> 0) then
      print *,"fail to open the file"
      print *,"iostat=",ios
      print *,"iomsg=",msg
      stop 10
  end if

  select type(poly1)
     type is(child(*,*))

         call poly1%writechild

         rewind 10

         call poly1%readchild

         write(*,'(2l2/3a3/5i3)',advance='yes')  poly1

     class default
         stop 11
  end select

  close(10,status='keep')

end program
