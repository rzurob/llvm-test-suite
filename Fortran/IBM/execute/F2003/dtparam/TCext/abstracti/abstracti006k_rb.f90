! GM DTP extension using:
! ftcx_dtp -qk -qnodeferredlp -qreuse=base /tstdev/F2003/abstracti/functional/abstracti006.f

!*  ===================================================================
!*
!*  TEST CASE NAME             : abstracti006k_rb
!*
!*                               by Alberto Alvarez-Mesquida)
!*  DATE                       : 2007-10-10 (original: 02/20/2006)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : Abstract Interface
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*
!*  DESCRIPTION                : Section 4.5.4 Type Bound Procedures (generic
!*  -binding)
!*  - Specific Binding
!*  - deferred specific type bound procedure
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*
!* =====================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

   type, abstract :: base(k1,n1)    ! (4,3)
      integer, kind :: k1
      integer, len  :: n1
      character(n1) :: c = 'xxx'
      contains
         procedure(winf), deferred, pass :: write
         procedure(rinf), deferred, pass :: read
         generic :: write(formatted) => write
         generic :: read(formatted)  => read
   end type

   type, extends(base) :: child    ! (4,3)
      integer(k1) :: i = -999
      contains
         procedure, pass :: write => writechild
         procedure, pass :: read  => readchild
   end type

   abstract interface
      subroutine winf (dtv, unit, iotype, v_list, iostat, iomsg)
         import base
         class(base(4,*)), intent(in) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine
   end interface

   abstract interface
      subroutine rinf (dtv, unit, iotype, v_list, iostat, iomsg)
         import base
         class(base(4,*)), intent(inout) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine
   end interface

   contains

      subroutine writechild (dtv, unit, iotype, v_list, iostat, iomsg)
         class(child(4,*)), intent(in) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, "(A3,1X,I4)", iostat=iostat, iomsg=iomsg) dtv%c, dtv%i
         iomsg = 'dtiowrite'

      end subroutine

      subroutine readchild (dtv, unit, iotype, v_list, iostat, iomsg)
         class(child(4,*)), intent(inout) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, "(A3,1X,I4)", iostat=iostat, iomsg=iomsg) dtv%c, dtv%i
         iomsg = 'dtioread'

      end subroutine

end module

program abstracti006k_rb
   use m

   integer(4) :: stat
   character(200) :: msg


   class(base(4,3)), allocatable :: b1
   class(base(4,3)), pointer     :: b2

   type(child(4,3)), target      :: c1
   class(child(4,3)), pointer    :: c2

   namelist /n1/ b1, c1
   namelist /n2/ b2, c2

   allocate ( b1, source = child(4,3) ( 'abc', 1001 ) )
   allocate ( b2, source = child(4,3) ( 'def', 1002 ) )
   c1 = child(4,3) ( 'ghi', 1003 )
   allocate ( c2, source = child(4,3) ( 'jkl', 1004 ) )

   open ( 1, file = 'abstracti006k_rb.1', form='formatted', access='sequential' )

   write ( 1, n1, iostat = stat, iomsg = msg )
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4

   write ( 1, n2, iostat = stat, iomsg = msg )
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 2_4

   rewind 1

   deallocate ( b1, b2, c2 )
   allocate ( child(4,3) :: b1, b2, c2 )
   c1 = child(4,3)()

   read ( 1, n1, iostat = stat, iomsg = msg )
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 3_4

   read ( 1, n2, iostat = stat, iomsg = msg )
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 4_4

   select type ( b1 )
      type is ( child(4,*) )
         if ( ( b1%c /= 'abc' ) .or. (b1%i /= 1001 ) ) error stop 5_4
   end select

   select type ( b2 )
      type is ( child(4,*) )
         if ( ( b2%c /= 'def' ) .or. (b2%i /= 1002 ) ) error stop 6_4
   end select

   if ( ( c1%c /= 'ghi' ) .or. ( c1%i /= 1003 ) ) error stop 7_4
   if ( ( c2%c /= 'jkl' ) .or. ( c2%i /= 1004 ) ) error stop 8_4

   deallocate ( b2, c2 )
   allocate ( c2 )
   b2 => c2

   rewind 1

   read ( 1, n2, iostat = stat, iomsg = msg )
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) )     error stop 9_4

   select type ( b2 )
      type is ( child(4,*) )
         if ( ( b2%c /= 'jkl' ) .or. (b2%i /= 1004 ) ) error stop 10_4
   end select

   if ( ( c2%c /= 'jkl' ) .or. ( c2%i /= 1004 ) )      error stop 11_4

end program abstracti006k_rb
