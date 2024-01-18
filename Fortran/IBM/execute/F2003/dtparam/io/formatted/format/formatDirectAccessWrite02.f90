!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : formatDirectAccessWrite02.f
!*
!*  DATE                       : Dec. 14 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : FORMATTED INTRINSIC IO
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!* 1. test Write statement
!* 2. derived type has type bound procedure & generic binding
!* 3. derived type is unlimited polymorphic type
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type inner(l1)
      integer,len   :: l1
      character(5)  :: i1(4) ! l1=4
      character(4) :: c1(4)
      contains
        procedure,pass :: writeInner1
        procedure,pass :: writeInner2
        generic  :: writeInner=>writeInner1,writeInner2
   end type

   type outer(l2)
      integer,len :: l2
      real        :: r1(3) ! l2=3
      type(inner(3+1)) :: comp
      contains
        procedure,pass :: writeOuter1
        procedure,pass :: writeOuter2
        generic :: writeOuter =>writeOuter1,writeOuter2
   end type

   contains

      subroutine writeInner1(this,unit)
         class(inner(*)),intent(in) :: this
         integer,intent(in)  :: unit
         select type(this)
           type is(inner(*))
             write(unit,'(4a5/4a4)',rec=5) this
           class default
             stop 10
         end select
      end subroutine

      subroutine writeInner2(this,inner,unit)
         class(inner(*)),intent(inout) :: this
         type(inner(*)),intent(in)     :: inner
         integer,intent(in)            :: unit

         select type(this)
            type is(inner(*))
               this=inner
               write(unit,'(4a5/4a4)',rec=8) this
            class default
               stop 11
         end select
      end subroutine

      subroutine writeOuter1(this,unit)
         class(outer(*)),intent(in) :: this
         integer,intent(in) :: unit

         select type(this)
           type is(outer(*))
             write(unit,'(f10.1/e15.3/f10.3)',rec=11) this%r1
             call this%comp%writeInner(unit)
           class default
             stop 12
         end select

      end subroutine

      subroutine writeOuter2(this,outer,unit)
         class(outer(*)),intent(inout) :: this
         type(outer(*)),intent(in)     :: outer
         integer,intent(in)            :: unit

         select type(this)
            type is(outer(*))
               this=outer
               write(unit,'(f10.1/e15.3/f10.3)',rec=15) this%r1
               call this%comp%writeInner(outer%comp,unit)
            class default
               stop 13
         end select
      end subroutine

end module

program formatDirectAccessWrite02
  use m
  implicit none

  integer :: ios,unit
  character(300) :: msg

  class(*),pointer :: outer1=>null(),outer2=>null()

  allocate(outer1,source=outer(3)(r1=[+12345.6,-12345.6,-123.456],&
      comp=inner(4)(i1=[" +011"," +012"," +013"," +014"],c1=["abc","def","ghi","jkl"])) )

  allocate(outer2,source=outer(3)(r1=[+0.123456,-0.123456,+123.456],&
      comp=inner(4)(i1=[" -011"," -012"," -013"," -014"],c1=["ABC","DEF","GHI","JKL"])))

  unit=10

  open(unit,file="formatDirectAccessWrite02.out",form="formatted",&
       action='write',access='direct',recl=20, &
       blank='zero',sign='plus',decimal='comma',iostat=ios,iomsg=msg)

  if(ios /= 0) then
     print *,"fail to open the file"
     print *,"iostat=",ios
     print *,"iomsg=",msg
     stop 9
  else
     select type(outer1)
       type is(outer(*))
          call outer1%writeOuter(unit)
          select type(outer2)
             type is(outer(*))
                 call outer1%writeOuter(outer2,unit)
             class default
                stop 14
          end select
       class default
          stop 15
     end select

  end if

  close(10)

end program
