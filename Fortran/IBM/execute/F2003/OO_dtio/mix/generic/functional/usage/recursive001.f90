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
!*                                  - Recursive Formatted I/O with Non-polymorphic linked-list
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
      type(base), pointer :: next => null()
      contains
         procedure, pass :: write => writeb
         procedure, pass :: read => readb
         generic :: write(formatted) => write
         generic :: read(formatted)  => read
   end type

   contains

      recursive subroutine writeb (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base), intent(in) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, "(A3)", iostat=iostat, iomsg=iomsg) dtv%c

         if ( associated(dtv%next) ) then
            write (unit, "(DT)", iostat=iostat, iomsg=iomsg)    dtv%next
            if ( ( iostat /= 0 ) .or. ( iomsg /= 'dtiowriteb' ) ) error stop 1_4
         end if

         iomsg = 'dtiowriteb'

      end subroutine

      recursive subroutine readb (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base), intent(inout) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, "(A3)" , iostat=iostat, iomsg=iomsg) dtv%c

         if ( associated(dtv%next) ) then
            read (unit, "(DT)", iostat=iostat, iomsg=iomsg)     dtv%next
            if ( ( iostat /= 0 ) .or. ( iomsg /= 'dtioreadb' ) )  error stop 2_4
         end if

         iomsg = 'dtioreadb'

      end subroutine

end module

program recursive001
   use m

   class(base), allocatable, target :: b1
   type(base), target               :: b2

   type(base), pointer :: dummy

   namelist /n1/ b1

   integer :: stat
   character(200) :: msg

   allocate ( b1, source = base ( 'ABC' ) )
   allocate ( b1%next, source = base ( 'DEF' ) )
   allocate ( b1%next%next, source = base ( 'GHI' ) )
   allocate ( b1%next%next%next, source = base ( 'JKL' ) )
   allocate ( b1%next%next%next%next, source = base ( 'MNO' ) )
   allocate ( b1%next%next%next%next%next, source = base ( 'PQR' ) )

   b2 = base('abc')
   b2%next => b1%next

   open ( 1, file='recursive001.1', form='formatted', access='sequential' )

   write ( 1, n1, iostat = stat, iomsg = msg )
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) )   error stop 3_4

   write ( 1, *, iostat = stat, iomsg = msg )          b2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) )   error stop 4_4

   allocate ( b2%next, source = base('def') )
   allocate ( b2%next%next, source = base('ghi') )
   allocate ( b2%next%next%next, source = base('jkl') )
   allocate ( b2%next%next%next%next, source = base('mno') )
   allocate ( b2%next%next%next%next%next, source = base('pqr') )

   write ( 1, "(DT,DT)", iostat = stat, iomsg = msg )  b1, b2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) )   error stop 5_4

   rewind 1

   dummy => b1
   do while ( associated (dummy) )
      dummy%c = 'xxx'
      dummy => dummy%next
   end do

   dummy => b2
   do while ( associated (dummy) )
      dummy%c = 'xxx'
      dummy => dummy%next
   end do

   read ( 1, n1, iostat = stat, iomsg = msg )
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadb' ) )    error stop 6_4

   dummy => b1
   do while ( associated (dummy) )
      print *,dummy%c
      dummy => dummy%next
   end do

   read ( 1, "(1X,DT)", iostat = stat, iomsg = msg )   b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadb' ) )    error stop 7_4

   dummy => b1
   do while ( associated (dummy) )
      print *,dummy%c
      dummy => dummy%next
   end do

   read ( 1, "(DT,DT)", iostat = stat, iomsg = msg )   b2, b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadb' ) )    error stop 8_4

   dummy => b1
   do while ( associated (dummy) )
      print *,dummy%c
      dummy => dummy%next
   end do

   dummy => b2
   do while ( associated (dummy) )
      print *,dummy%c
      dummy => dummy%next
   end do

   close ( 1, status ='delete')

end program
