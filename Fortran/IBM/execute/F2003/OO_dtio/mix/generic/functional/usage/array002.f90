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
!*                                  - array non-polymorphic derived type variable (does not have DTIO)
!*                                    containing non-polymorphic components which has DTIO with formatted I/O
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
         generic :: write(formatted) => write
         generic :: read(formatted)  => read
   end type

   type container
      integer :: i = -999
      type(base) :: b = base()
   end type

   contains

      subroutine writeb (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base), intent(in) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, "(A3)", iostat=iostat, iomsg=iomsg) dtv%c

         iomsg = 'dtiowriteb'

      end subroutine

      subroutine readb (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base), intent(inout) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, "(A3)" , iostat=iostat, iomsg=iomsg) dtv%c
         iomsg = 'dtioreadb'

      end subroutine

end module

program array002
   use m

   type(container), allocatable :: b1(:)
   type(container) :: b2(2,2) = reshape( source = (/ container(201, base('ABC')), container(202, base('DEF')), container(203, base('GHI')), container(204, base('JKL')) /), &
                                         shape = (/ 2,2 /) )
   integer :: stat
   character(200) :: msg

   open ( 1, file = 'array002.1', form='formatted', access='sequential' )

   allocate ( b1(3), source = (/ container( 101, base('abc') ), container( 102, base('def') ), container( 103, base('ghi') ) /) )

   write ( 1, *, iostat = stat, iomsg = msg )             b1((/1,2,3/))
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) )      error stop 1_4

   write ( 1, "(I4,1X,DT)", iostat = stat, iomsg = msg )  b2             !<- reversion occurs
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) )      error stop 2_4

   rewind 1

   deallocate ( b1 )
   allocate ( b1(3) )

   b2 = container()

   read ( 1, *, iostat = stat, iomsg = msg )          b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadb' ) )  error stop 3_4

   read ( 1, "(I4,1X,DT,/,I4,1X,DT,/,I4,1X,DT,/,I4,1X,DT)", iostat = stat, iomsg = msg )   b2(1:2,1:2)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadb' ) )                                        error stop 4_4

   if ( ( b1(1)%i /= 101 ) .or. ( b1(1)%b%c /= 'abc' ) .or. &
        ( b1(2)%i /= 102 ) .or. ( b1(2)%b%c /= 'def' ) .or. &
        ( b1(3)%i /= 103 ) .or. ( b1(3)%b%c /= 'ghi' ) )    error stop 5_4

   if ( ( b2(1,1)%i /= 201 ) .or. ( b2(1,1)%b%c /= 'ABC' ) .or. &
        ( b2(2,1)%i /= 202 ) .or. ( b2(2,1)%b%c /= 'DEF' ) .or. &
        ( b2(1,2)%i /= 203 ) .or. ( b2(1,2)%b%c /= 'GHI' ) .or. &
        ( b2(2,2)%i /= 204 ) .or. ( b2(2,2)%b%c /= 'JKL' ) )    error stop 6_4

   close ( 1, status ='delete')

end program
