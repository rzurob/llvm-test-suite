!*  ===================================================================
!*
!*  DATE                       : 04/26/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Usage of GENERIC BINDING
!*                                  - scalar derived type containing private components
!*                                    with unformatted I/O
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
         generic :: write(unformatted) => write
         generic :: read(unformatted)  => read

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

      subroutine writeb (dtv, unit, iostat, iomsg)
         class(base), intent(in) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, iostat=iostat, iomsg=iomsg) dtv%getc()

         iomsg = 'dtiowriteb'

      end subroutine

      subroutine readb (dtv, unit, iostat, iomsg)
         class(base), intent(inout) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         character(3) :: c
         read (unit, iostat=iostat, iomsg=iomsg) c
         call dtv%setc(c)

         iomsg = 'dtioreadb'

      end subroutine

      subroutine writec (dtv, unit, iostat, iomsg)
         class(child), intent(in) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, iostat=iostat, iomsg=iomsg) dtv%getc(), dtv%i

         iomsg = 'dtiowritec'

      end subroutine

      subroutine readc (dtv, unit, iostat, iomsg)
         class(child), intent(inout) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         character(3) :: c
         read (unit, iostat=iostat, iomsg=iomsg) c, dtv%i

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

program scalar009a
   use m

   class(base) , allocatable :: b1
   class(child), pointer     :: c1

   integer :: stat
   character(200) :: msg

   open ( 1, file = 'scalar009a.1', form='unformatted', access='sequential' )

   allocate ( b1, c1 )

   call b1%setc('abc')
   call c1%setc('def')
   call c1%seti(1001)

   write ( 1, iostat = stat, iomsg = msg ) b1, c1

   deallocate ( b1 )
   allocate ( child :: b1 )

   select type ( b1 )
      type is ( child )
         call b1%setc('ghi')
         call b1%seti(1002)
   end select

   write ( 1, iostat = stat, iomsg = msg ) b1

   rewind 1

   deallocate ( b1, c1 )
   allocate ( b1, c1 )

   read ( 1, iostat = stat, iomsg = msg ) b1, c1

   if ( ( b1%getc() /= 'abc' ) .or. ( c1%getc() /= 'def' ) .or. ( c1%geti() /= 1001 ) ) error stop 1_4

   deallocate ( b1 )
   allocate ( child :: b1 )

   read ( 1, iostat = stat, iomsg = msg )  b1

   select type ( b1 )
      type is ( child )
         if ( ( b1%getc() /= 'ghi' ) .or. ( b1%geti() /= 1002 ) ) error stop 2_4
   end select

   close ( 1, status ='delete')

end program