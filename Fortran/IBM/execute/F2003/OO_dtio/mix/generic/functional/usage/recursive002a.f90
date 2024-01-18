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
!*                                  - Recursive unformatted I/O with Polymorphic linked-list
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
      class(base), pointer :: next => null()
      contains
         procedure, pass :: write => writeb
         procedure, pass :: read => readb
         generic :: write(unformatted) => write
         generic :: read(unformatted)  => read
   end type

   type, extends(base) :: child
      integer(4) :: i = -999
      contains
         procedure, pass :: read => readc
   end type

   contains

      recursive subroutine writeb (dtv, unit, iostat, iomsg)
         class(base), intent(in) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         select type ( dtv )
            type is ( base )
               write (unit, iostat=iostat, iomsg=iomsg) dtv%c
            type is ( child )
               write (unit, iostat=iostat, iomsg=iomsg) dtv%c, dtv%i
         end select

         if ( associated(dtv%next) ) then
            write (unit, iostat=iostat, iomsg=iomsg)      dtv%next
            if ( ( iostat /= 0 ) .or. ( iomsg /= 'dtiowrite' ) )  error stop 1_4
         end if

         iomsg = 'dtiowrite'

      end subroutine

      recursive subroutine readb (dtv, unit, iostat, iomsg)
         class(base), intent(inout) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, iostat=iostat, iomsg=iomsg) dtv%c

         if ( associated(dtv%next) ) then
            read (unit, iostat=iostat, iomsg=iomsg)       dtv%next
            if ( ( iostat /= 0 ) .or. ( iomsg /= 'dtioread' ) )   error stop 2_4
         end if

         iomsg = 'dtioread'

      end subroutine

      recursive subroutine readc (dtv, unit, iostat, iomsg)
         class(child), intent(inout) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, iostat=iostat, iomsg=iomsg)   dtv%c, dtv%i

         if ( associated(dtv%next) ) then
            read (unit, iostat=iostat, iomsg=iomsg)      dtv%next
            if ( ( iostat /= 0 ) .or. ( iomsg /= 'dtioread' ) )  error stop 3_4
         end if

         iomsg = 'dtioread'

      end subroutine

end module

program recursive002a
   use m

   class(base), allocatable, target :: b1
   class(base), pointer             :: b2

   class(base), pointer :: dummy

   namelist /n1/ b1

   integer :: stat
   character(200) :: msg

   allocate ( b1, source = child ( c='ABC',i= 101 ) )
   allocate ( b1%next, source = child (c= 'DEF', i=102 ) )
   allocate ( b1%next%next, source = base (c= 'GHI' ) )
   allocate ( b1%next%next%next, source = base (c= 'JKL' ) )
   allocate ( b1%next%next%next%next, source = child (c= 'MNO', i=103 ) )
   allocate ( b1%next%next%next%next%next, source = child (c= 'PQR',i= 104 ) )

   allocate ( b2, source = child(c='abc',i=105) )
   b2%next => b1%next

   open ( 1, file='recursive002a.1', form='unformatted', access='sequential' )

   write ( 1, iostat = stat, iomsg = msg )             b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) )   error stop 4_4

   write ( 1, iostat = stat, iomsg = msg )             b2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) )   error stop 5_4

   allocate ( b2%next, source = child(c='def',i=201 ) )
   allocate ( b2%next%next, source = base(c='ghi') )
   allocate ( b2%next%next%next, source = child(c='jkl',i=202 ) )
   allocate ( b2%next%next%next%next, source = base(c='mno') )
   allocate ( b2%next%next%next%next%next, source = child(c='pqr',i=203 ) )

   write ( 1, iostat = stat, iomsg = msg )               b1, b2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) )     error stop 6_4

   rewind 1

   dummy => b1
   do while ( associated (dummy) )
      select type ( dummy )
         type is ( base )
            dummy%c = 'xxx'
         type is ( child )
            dummy%c = 'xxx'
            dummy%i = -999
      end select
      dummy => dummy%next
   end do

   dummy => b2
   do while ( associated (dummy) )
      select type ( dummy )
         type is ( base )
            dummy%c = 'xxx'
         type is ( child )
            dummy%c = 'xxx'
            dummy%i = -999
      end select
      dummy => dummy%next
   end do

   read ( 1, iostat = stat, iomsg = msg )              b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) )    error stop 7_4

   dummy => b1
   do while ( associated (dummy) )
      select type ( dummy )
         type is ( base )
            print *, dummy%c
         type is ( child )
            print *, dummy%c, dummy%i
      end select
      dummy => dummy%next
   end do

   read ( 1, iostat = stat, iomsg = msg )              b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) )    error stop 8_4

   dummy => b1
   do while ( associated (dummy) )
      select type ( dummy )
         type is ( base )
            print *, dummy%c
         type is ( child )
            print *, dummy%c, dummy%i
      end select
      dummy => dummy%next
   end do

   read ( 1, iostat = stat, iomsg = msg )              b1, b2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) )    error stop 9_4

   dummy => b1
   do while ( associated (dummy) )
      select type ( dummy )
         type is ( base )
            print *, dummy%c
         type is ( child )
            print *, dummy%c, dummy%i
      end select
      dummy => dummy%next
   end do

   dummy => b2
   do while ( associated (dummy) )
      select type ( dummy )
         type is ( base )
            print *, dummy%c
         type is ( child )
            print *, dummy%c, dummy%i
      end select
      dummy => dummy%next
   end do

   close ( 1, status ='delete')

end program
