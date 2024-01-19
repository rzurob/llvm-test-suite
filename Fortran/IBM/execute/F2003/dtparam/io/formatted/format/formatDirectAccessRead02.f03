!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Dec. 15 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : FORMATTED INTRINSIC IO
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!* 1. test READ statement
!* 2. derived type has type bound procedure & generic binding
!* 3. derived type is unlimited polymorphic type
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type inner(k1,l1)
      integer,kind  :: k1
      integer,len   :: l1
      integer(k1)   :: i1(l1) ! l1=4
      character(l1) :: c1(l1)
      contains
        procedure,nopass :: readInner4
        procedure,nopass :: readInner8
        generic  :: readInner=>readInner4,readInner8
   end type

   type outer(k2,l2)
      integer,kind :: k2
      integer,len  :: l2
      real(k2+k2)     :: r1(l2) ! l2=3
      type(inner(k2,l2+1)) :: comp
      contains
        procedure,nopass :: readOuter4
        procedure,nopass :: readOuter8
        generic :: readOuter =>readOuter4,readOuter8
   end type

   type(outer(2,3)) :: dtp

   contains

      subroutine readInner4(inner4,unit)
         class(inner(4,*)),intent(inout) :: inner4
         integer,intent(in)  :: unit
         select type(inner4)
           type is(inner(4,*))
             read(unit,'(4a4)',rec=10) inner4%c1
             read(unit,'(i3,i4,i5,i6)',rec=11) inner4%i1
           class default
             stop 10
         end select
      end subroutine

      subroutine readInner8(inner8,unit)
         class(inner(8,*)),intent(inout) :: inner8
         integer,intent(in)  :: unit
         select type(inner8)
           type is(inner(8,*))
            read(unit,'(bn,i3,i4,i5,i6)',rec=16) inner8%i1
            read(unit,'(4a4)',rec=17) inner8%c1
           class default
            stop 11
         end select
      end subroutine

      subroutine readOuter4(outer4,unit)
         class(outer(4,*)),intent(inout) :: outer4
         integer,intent(in) :: unit

         select type(outer4)
           type is(outer(4,*))
             read(unit,'(f9.3,/f9.3,/f9.4)',rec=5) outer4%r1
             call dtp%comp%readInner4(outer4%comp,unit)
           class default
             stop 12
         end select

      end subroutine

      subroutine readOuter8(outer8,unit)
         class(outer(8,*)),intent(inout) :: outer8
         integer,intent(in) :: unit

         select type(outer8)
           type is(outer(8,*))
             read(unit,'(f8.3,/f8.3,/f9.4)',rec=13) outer8%r1
             call dtp%comp%readInner8(outer8%comp,unit)
           class default
             stop 13
         end select

      end subroutine

end module

program formatDirectAccessRead02
  use m
  implicit none

  integer :: ios,unit
  character(300) :: msg

  class(*),pointer :: outer4=>null(),outer8=>null()

  allocate(outer(4,3) :: outer4)
  allocate(outer(8,3) :: outer8)

  unit=10

  open(unit,file="formatDirectAccessRead02.in",form="formatted",&
       action='read',access='direct',recl=20, &
       blank='zero',iostat=ios,iomsg=msg)

  if(ios /= 0) then
     print *,"fail to open the file"
     print *,"iostat=",ios
     print *,"iomsg=",msg
     stop 8
  else
    select type(outer4)
      type is(outer(4,*))
        call dtp%readOuter(outer4,unit)
        write(*,'(a)') "outer4:"
        write(*,100) outer4
      class default
        stop 14
    end select

   select type(outer8)
     type is(outer(8,*))
       call dtp%readOuter(outer8,unit)
        write(*,'(a)') "outer8:"
        write(*,100) outer8
     class default
       stop 15
   end select

  end if

100 format(f8.3,/f8.3,/f8.3,/i3,i4,i5,i6,/4a4)

  close(10)

end program
