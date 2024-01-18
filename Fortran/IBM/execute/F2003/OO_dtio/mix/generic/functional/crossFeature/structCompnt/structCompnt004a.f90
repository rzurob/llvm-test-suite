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
!*  DESCRIPTION                : GENERIC BINDING:
!*                                  Cross Feature: Structure Component
!*                                    -  non-polymorphic structure array component with unformatted i/o
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
         generic :: read(unformatted) => read
   end type

   type container
      type(base) :: b1(3)
      type(base), allocatable :: b2(:)
   end type

   contains

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

end module

program structCompnt004a
   use m

   integer :: stat
   character(200) :: msg

   type(container) :: cc1
   class(container), allocatable :: cc2

   open ( 1, file = 'structCompnt004a.1', form='unformatted', access='sequential' )

   cc1 = container( (/ base('abc'), base('def'), base('ghi') /), (/ base('ABC'), base('DEF') /) )
   allocate ( cc2, source = container( (/ base('101'), base('102'), base('103') /), (/ base('201'), base('202'), base('203') /) ) )

   write ( 1, iostat = stat, iomsg = msg )     cc1%b1, cc1%b2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) )         error stop 1_4

   write ( 1, iostat = stat, iomsg = msg )     cc2%b1, cc2%b2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) )         error stop 2_4

   rewind 1

   deallocate ( cc2 )
   cc1 = container( (/ ( base('xxx') , i=1,3 )  /), (/ ( base('xxx') , i=1,2)  /) )
   allocate ( cc2, source = container( (/ ( base('xxx') , i=1,3 )  /), (/ ( base('xxx') , i=1,3)  /) ) )

   read ( 1, iostat = stat, iomsg = msg )      cc1%b1, cc1%b2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadb' ) )          error stop 3_4

   if ( ( cc1%b1(1)%c /= 'abc' ) .or. ( cc1%b1(2)%c /= 'def' ) .or. ( cc1%b1(3)%c /= 'ghi' ) .or. &
        ( cc1%b2(1)%c /= 'ABC' ) .or. ( cc1%b2(2)%c /= 'DEF' ) )                                  error stop 4_4

   read ( 1, iostat = stat, iomsg = msg )      cc2%b1, cc2%b2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadb' ) )          error stop 5_4

   if ( ( cc2%b1(1)%c /= '101' ) .or. ( cc2%b1(2)%c /= '102' ) .or. ( cc2%b1(3)%c /= '103' ) .or. &
        ( cc2%b2(1)%c /= '201' ) .or. ( cc2%b2(2)%c /= '202' ) .or. ( cc2%b2(3)%c /= '203' ) )    error stop 5_4

   close ( 1, status ='delete')

end program
