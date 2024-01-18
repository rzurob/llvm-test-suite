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
!*  DESCRIPTION                : Section 9.5.3.7.3 Resolving derived-type input/output procedure references (generic binding)
!*                                    - Make both generic type bound and interface available
!*                                        - for unformatted I/O with single non-extended type, recursive I/O
!*                                          which contains dtio interface (which shall never be called)
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
         procedure, pass :: write => writebase
         procedure, pass :: read => readbase
         generic :: write(unformatted) => write
         generic :: read(unformatted)  => read
   end type

   contains

      recursive subroutine writebase (dtv, unit, iostat, iomsg)
         class(base), intent(in) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         interface write(unformatted)
            subroutine writebaseext (dtv, unit, iostat, iomsg)
               import base
               class(base), intent(in) :: dtv
               integer, intent(in) :: unit
               integer, intent(out) :: iostat
               character(*), intent(inout) :: iomsg
            end subroutine
         end interface

         write (unit, iostat=iostat, iomsg=iomsg) dtv%c

         if ( associated ( dtv%next ) ) then
            write (unit, iostat = iostat, iomsg = iomsg ) dtv%next
         end if

         iomsg = 'dtiowrite'

      end subroutine

      recursive subroutine readbase (dtv, unit, iostat, iomsg)
         class(base), intent(inout) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         interface read(unformatted)
            subroutine readbaseext (dtv, unit, iostat, iomsg)
               import base
               class(base), intent(inout) :: dtv
               integer, intent(in) :: unit
               integer, intent(out) :: iostat
               character(*), intent(inout) :: iomsg
            end subroutine
         end interface

         read (unit, iostat=iostat, iomsg=iomsg) dtv%c

         if ( associated ( dtv%next ) ) then
            read (unit, iostat = iostat, iomsg = iomsg ) dtv%next
         end if

         iomsg = 'dtioread'

      end subroutine

end module

program resolve006a
   use m

   class(base), allocatable :: b1
   type(base) , pointer     :: b2, dummy

   integer :: stat
   character(200) :: msg

   allocate ( b1, source = base ( 'abc' ) )
   allocate ( b1%next, source = base ('def') )
   allocate ( b1%next%next, source = base ('ghi') )
   allocate ( b1%next%next%next, source = base ('jkl') )
   allocate ( b1%next%next%next%next, source = base ('mno') )
   allocate ( b1%next%next%next%next%next, source = base ('pqr') )

   allocate ( b2, source = base () )
   allocate ( b2%next, source = base () )
   allocate ( b2%next%next, source = base () )
   allocate ( b2%next%next%next, source = base () )
   allocate ( b2%next%next%next%next, source = base () )
   allocate ( b2%next%next%next%next%next, source = base () )

   open ( 1, file = 'resolve006a.1', form='unformatted', access='sequential' )

   write ( 1, iostat = stat, iomsg = msg ) b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4
   
   rewind 1
   
   read ( 1, iostat = stat, iomsg = msg )  b2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) )  error stop 2_4
   
   dummy => b2
   
   do while ( associated(dummy ) )
      print *, dummy%c
      dummy => dummy%next   
   end do
   
   close ( 1, status ='delete')

end program

subroutine writebaseext (dtv, unit, iostat, iomsg)
   use m
   class(base), intent(in) :: dtv
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   error stop 10_4
   iomsg = 'ERROR'

end subroutine

subroutine readbaseext (dtv, unit, iostat, iomsg)
   use m
   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   error stop 11_4
   iomsg = 'ERROR'

end subroutine
