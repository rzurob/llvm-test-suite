!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 04/26/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : Usage of GENERIC BINDING
!*                                  - scalar (non-) polymorphic derived type entity with DTIO
!*                                    containing components which has DTIO procedure with unformatted I/O
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
      character(3) :: c = 'xxx'
      contains
         procedure, pass :: write => writeb
         procedure, pass :: read => readb
         generic :: write(unformatted) => write
         generic :: read(unformatted)  => read
   end type

   type, extends(base) :: child
      character(3) :: d = 'xxx'
      contains
         procedure, pass :: write => writec
         procedure, pass :: read => readc
   end type

   type :: container
      integer :: i
      class(base), allocatable :: b
      contains
         procedure, pass :: write => writecon
         procedure, pass :: read => readcon
         generic :: write(unformatted) => write
         generic :: read(unformatted)  => read
   end type

   contains

      subroutine writecon (dtv, unit, iostat, iomsg)
         class(container), intent(in) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, iostat=iostat, iomsg=iomsg) dtv%i, dtv%b

         select type ( g => dtv%b )
            type is ( base )
               if ( ( iostat /= 0 ) .or. ( iomsg /= 'dtiowriteb' ) ) error stop 1_4
            type is ( child )
               if ( ( iostat /= 0 ) .or. ( iomsg /= 'dtiowritec' ) ) error stop 2_4
         end select

         iomsg = 'dtiowritecon'

      end subroutine

      subroutine readcon (dtv, unit, iostat, iomsg)
         class(container), intent(inout) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, iostat=iostat, iomsg=iomsg) dtv%i, dtv%b

         select type ( g => dtv%b )
            type is ( base )
               if ( ( iostat /= 0 ) .or. ( iomsg /= 'dtioreadb' ) ) error stop 3_4
            type is ( child )
               if ( ( iostat /= 0 ) .or. ( iomsg /= 'dtioreadc' ) ) error stop 4_4
         end select

         iomsg = 'dtioreadcon'


      end subroutine

      subroutine writeb (dtv, unit, iostat, iomsg)
         class(base), intent(in) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, iostat=iostat, iomsg=iomsg) dtv%c
         iomsg = 'dtiowriteb'

      end subroutine

      subroutine readb (dtv, unit, iostat, iomsg)
         class(base), intent(inout) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, iostat=iostat, iomsg=iomsg) dtv%c
         iomsg = 'dtioreadb'

      end subroutine

      subroutine writec (dtv, unit, iostat, iomsg)
         class(child), intent(in) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, iostat=iostat, iomsg=iomsg) dtv%c, dtv%d
         iomsg = 'dtiowritec'

      end subroutine

      subroutine readc (dtv, unit, iostat, iomsg)
         class(child), intent(inout) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, iostat=iostat, iomsg=iomsg) dtv%c, dtv%d
         iomsg = 'dtioreadc'

      end subroutine

end module

program scalar003a
   use m

   class(container), allocatable :: c1
   type(container)               :: c2

   integer :: stat
   character(200) :: msg

   allocate ( c1, source = container(101, base('abc') ) )
   c2 = container(102, base('def') )

   open ( 1, file = 'scalar003a.1', form='unformatted', access='sequential' )

   write ( 1, iostat = stat, iomsg = msg )    c1, c2

   deallocate ( c1, c2%b )

   allocate ( c1, source = container(103, child('ABC','DEF') ) )
   c2 = container(104, child('GHI','JKL') )

   write ( 1, iostat = stat, iomsg = msg )            c1, c2

   rewind 1

   deallocate ( c1, c2%b )

   allocate ( c1, source = container(-999, base('xxx') ) )
   c2 = container(-999, base('xxx') )

   read  ( 1, iostat = stat, iomsg = msg )    c2, c1

   if ( ( c1%i /= 102 ) .or. ( c1%b%c /= 'def' ) .or. &
        ( c2%i /= 101 ) .or. ( c2%b%c /= 'abc' )      ) error stop 5_4

   deallocate ( c1, c2%b )
   allocate ( c1, source = container(-999, child('xxx','xxx') ) )
   c2 = container(-999, child('xxx','xxx') )

   read  ( 1, iostat = stat, iomsg = msg )    c2, c1

   select type ( g => c1%b )
      type is ( child )
         select type ( h => c2%b )
            type is ( child )
               if ( ( c1%i /= 104 ) .or. ( g%c /= 'GHI' ) .or. ( g%d /= 'JKL' ) .or. &
                    ( c2%i /= 103 ) .or. ( h%c /= 'ABC' ) .or. ( h%d /= 'DEF' )      ) error stop 6_4
         end select
   end select

   close ( 1, status ='delete')

end program
