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
!*                                  - Recursive unformatted I/O with linked-list that contains
!*                                    components which requires DTIO
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

   type item
      character(3) :: c = 'xxx'
      contains
         procedure, pass :: write => writei
         procedure, pass :: read => readi
         generic :: write(unformatted) => write
         generic :: read(unformatted)  => read      
   end type

   type base
      type(item) :: i = item()
      type(base), pointer :: next => null()
      contains
         procedure, pass :: write => writeb
         procedure, pass :: read => readb
         generic :: write(unformatted) => write
         generic :: read(unformatted)  => read
   end type

   contains

      subroutine writei (dtv, unit, iostat, iomsg)
         class(item), intent(in) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, iostat=iostat, iomsg=iomsg) dtv%c
         iomsg = 'dtiowritei'

      end subroutine

      subroutine readi (dtv, unit, iostat, iomsg)
         class(item), intent(inout) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, iostat=iostat, iomsg=iomsg) dtv%c
         iomsg = 'dtioreadi'

      end subroutine

      recursive subroutine writeb (dtv, unit, iostat, iomsg)
         class(base), intent(in) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, iostat=iostat, iomsg=iomsg) dtv%i
         if ( ( iostat /= 0 ) .or. ( iomsg /= 'dtiowritei' ) )    error stop 1_4

         if ( associated(dtv%next) ) then
            write (unit, iostat=iostat, iomsg=iomsg)    dtv%next
            if ( ( iostat /= 0 ) .or. ( iomsg /= 'dtiowriteb' ) ) error stop 2_4
         end if

         iomsg = 'dtiowriteb'

      end subroutine

      recursive subroutine readb (dtv, unit, iostat, iomsg)
         class(base), intent(inout) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, iostat=iostat, iomsg=iomsg) dtv%i
         if ( ( iostat /= 0 ) .or. ( iomsg /= 'dtioreadi' ) )     error stop 3_4
         if ( associated(dtv%next) ) then
            read (unit, iostat=iostat, iomsg=iomsg)     dtv%next
            if ( ( iostat /= 0 ) .or. ( iomsg /= 'dtioreadb' ) )  error stop 4_4
         end if

         iomsg = 'dtioreadb'

      end subroutine

end module

program recursive003a
   use m

   class(base), allocatable, target :: b1
   type(base), target               :: b2

   type(base), pointer :: dummy

   namelist /n1/ b1

   integer :: stat
   character(200) :: msg

   allocate ( b1, source = base ( item('ABC') ) )
   allocate ( b1%next, source = base ( item('DEF') ) )
   allocate ( b1%next%next, source = base ( item('GHI') ) )
   allocate ( b1%next%next%next, source = base ( item('JKL') ) )
   allocate ( b1%next%next%next%next, source = base ( item('MNO') ) )
   allocate ( b1%next%next%next%next%next, source = base ( item('PQR') ) )

   b2 = base(item('abc'))
   b2%next => b1%next

   open ( 1, file='recursive003a.1', form='unformatted', access='sequential' )

   write ( 1, iostat = stat, iomsg = msg )             b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) )   error stop 3_4

   write ( 1, iostat = stat, iomsg = msg )             b2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) )   error stop 4_4

   allocate ( b2%next, source = base(item('def')) )
   allocate ( b2%next%next, source = base(item('ghi')) )
   allocate ( b2%next%next%next, source = base(item('jkl')) )
   allocate ( b2%next%next%next%next, source = base(item('mno')) )
   allocate ( b2%next%next%next%next%next, source = base(item('pqr')) )

   write ( 1, iostat = stat, iomsg = msg )             b1, b2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) )   error stop 5_4

   rewind 1

   dummy => b1
   do while ( associated (dummy) )
      dummy%i%c = 'xxx'
      dummy => dummy%next
   end do

   dummy => b2
   do while ( associated (dummy) )
      dummy%i%c = 'xxx'
      dummy => dummy%next
   end do

   read ( 1, iostat = stat, iomsg = msg )              b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadb' ) )    error stop 6_4

   dummy => b1
   do while ( associated (dummy) )
      print *,dummy%i%c
      dummy => dummy%next
   end do

   read ( 1, iostat = stat, iomsg = msg )              b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadb' ) )    error stop 7_4

   dummy => b1
   do while ( associated (dummy) )
      print *,dummy%i%c
      dummy => dummy%next
   end do

   read ( 1, iostat = stat, iomsg = msg )              b2, b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadb' ) )    error stop 8_4

   dummy => b1
   do while ( associated (dummy) )
      print *,dummy%i%c
      dummy => dummy%next
   end do

   dummy => b2
   do while ( associated (dummy) )
      print *,dummy%i%c
      dummy => dummy%next
   end do

   close ( 1, status ='delete')

end program
