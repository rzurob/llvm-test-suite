!*  ===================================================================
!*
!*  DATE                       : 04/26/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Usage of GENERIC BINDING
!*                                  - array derived type containing private components
!*                                    with formatted I/O
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

   type base
      character(3), private :: c = 'xxx'
      contains
         procedure, pass :: write => writeb
         procedure, pass :: read => readb
         generic :: write(formatted) => write
         generic :: read(formatted)  => read

         procedure, pass :: getc
         procedure, pass :: setc

   end type

   type, extends(base) :: child
      integer(4), private :: i = -999
      contains
         procedure, pass :: write => writec
         procedure, pass :: read => readc

         procedure, pass :: geti
         procedure, pass :: seti

   end type

   contains

      subroutine writeb (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base), intent(in) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, "(A3)", iostat=iostat, iomsg=iomsg) dtv%getc()

         iomsg = 'dtiowriteb'

      end subroutine

      subroutine readb (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base), intent(inout) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         character(3) :: c
         read (unit, "(A3)" , iostat=iostat, iomsg=iomsg) c
         call dtv%setc(c)

         iomsg = 'dtioreadb'

      end subroutine

      subroutine writec (dtv, unit, iotype, v_list, iostat, iomsg)
         class(child), intent(in) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, "(A3,1X,I4)", iostat=iostat, iomsg=iomsg) dtv%getc(), dtv%i

         iomsg = 'dtiowritec'

      end subroutine

      subroutine readc (dtv, unit, iotype, v_list, iostat, iomsg)
         class(child), intent(inout) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         character(3) :: c

         read (unit, "(A3,1X,I4)" , iostat=iostat, iomsg=iomsg) c, dtv%i

         call dtv%setc(c)

         iomsg = 'dtioreadc'

      end subroutine

      character(3) function getc(dtv)
         class(base), intent(in) :: dtv
         getc = dtv%c
      end function

      subroutine setc(dtv,cc)
         class(base), intent(inout) :: dtv
         character(3), intent(in) :: cc
         dtv%c = cc
      end subroutine

      integer(4) function geti(dtv)
         class(child), intent(in) :: dtv
         geti = dtv%i
      end function

      subroutine seti(dtv,ii)
         class(child), intent(inout) :: dtv
         integer(4), intent(in) :: ii
         dtv%i = ii
      end subroutine

end module

program array008
   use m

   class(base) , allocatable :: b1(:)
   class(child), pointer     :: c1(:,:)

   integer :: stat
   character(200) :: msg

   open ( 1, file = 'array008.1', form='formatted', access='sequential' )

   allocate ( b1(3), c1(2,2) )

   call b1(1)%setc('abc')
   call b1(2)%setc('def')
   call b1(3)%setc('ghi')

   call c1(1,1)%setc('ABC')
   call c1(1,1)%seti(101)
   call c1(2,1)%setc('DEF')
   call c1(2,1)%seti(102)
   call c1(1,2)%setc('GHI')
   call c1(1,2)%seti(103)
   call c1(2,2)%setc('JKL')
   call c1(2,2)%seti(104)

   write ( 1, "(7(DT))", iostat = stat, iomsg = msg ) b1, c1

   deallocate ( b1 )
   allocate ( child :: b1(4) )

   select type ( b1 )
      type is ( child )
         call b1(1)%setc('ABC')
         call b1(1)%seti(201)
         call b1(2)%setc('DEF')
         call b1(2)%seti(202)
         call b1(3)%setc('GHI')
         call b1(3)%seti(203)
         call b1(4)%setc('JKL')
         call b1(4)%seti(204)
   end select

   write ( 1, "(4(DT))", iostat = stat, iomsg = msg ) b1

   rewind 1

   deallocate ( b1, c1 )
   allocate ( b1(3), c1(2,2) )

   read ( 1, "(7(DT))", iostat = stat, iomsg = msg ) b1, c1

   if ( ( b1(1)%getc() /= 'abc' ) .or. ( b1(2)%getc() /= 'def' ) .or. ( b1(3)%getc() /= 'ghi' ) .or. &
        ( c1(1,1)%getc() /= 'ABC' ) .or. ( c1(1,1)%geti() /= 101 ) .or. &
        ( c1(2,1)%getc() /= 'DEF' ) .or. ( c1(2,1)%geti() /= 102 ) .or. &
        ( c1(1,2)%getc() /= 'GHI' ) .or. ( c1(1,2)%geti() /= 103 ) .or. &
        ( c1(2,2)%getc() /= 'JKL' ) .or. ( c1(2,2)%geti() /= 104 ) &
        ) error stop 1_4

   deallocate ( b1 )
   allocate ( child :: b1(4) )

   read ( 1, "(4(DT))", iostat = stat, iomsg = msg )  b1

   select type ( b1 )
      type is ( child )
         if ( ( b1(1)%getc() /= 'ABC' ) .or. ( b1(1)%geti() /= 201 ) .or. &
              ( b1(2)%getc() /= 'DEF' ) .or. ( b1(2)%geti() /= 202 ) .or. &
              ( b1(3)%getc() /= 'GHI' ) .or. ( b1(3)%geti() /= 203 ) .or. &
              ( b1(4)%getc() /= 'JKL' ) .or. ( b1(4)%geti() /= 204 ) ) error stop 2_4
   end select

   close ( 1, status ='delete')

end program
