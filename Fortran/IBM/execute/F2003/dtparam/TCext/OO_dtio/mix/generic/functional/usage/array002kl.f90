!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : array002kl
!*
!*  PROGRAMMER                 : David Forster (derived from array002 by Robert Ma)
!*  DATE                       : 2007-08-09 (original: 04/26/2005)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DRIVER STANZA              : xlf2003
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

   type base (lbase_1) ! lbase_1=3
      integer, len :: lbase_1
      character(lbase_1) :: c = 'xxx'
      contains
         procedure, pass :: write => writeb
         procedure, pass :: read => readb
         generic :: write(formatted) => write
         generic :: read(formatted)  => read
   end type

   type container (kcontainer_1,lcontainer_1) ! kcontainer_1,lcontainer_1=4,3
      integer, kind :: kcontainer_1
      integer, len :: lcontainer_1
      integer(kcontainer_1) :: i = -999
      type(base(lcontainer_1)) :: b! = base(lcontainer_1)() ! tcx: (lcontainer_1) ! tcx: (lcontainer_1)
   end type

   contains

      subroutine writeb (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base(*)), intent(in) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, "(A3)", iostat=iostat, iomsg=iomsg) dtv%c

         iomsg = 'dtiowriteb'

      end subroutine

      subroutine readb (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base(*)), intent(inout) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, "(A3)" , iostat=iostat, iomsg=iomsg) dtv%c
         iomsg = 'dtioreadb'

      end subroutine

end module

program array002kl
   use m

   type(container(4,:)), allocatable :: b1(:) ! tcx: (4,:)
   type(container(4,3)) :: b2(2,2) = reshape( source = (/ container(4,3)(201, base(3)('ABC')), container(4,3)(202, base(3)('DEF')), container(4,3)(203, base(3)('GHI')), container(4,3)(204, base(3)('JKL')) /), & ! tcx: (3) ! tcx: (3) ! tcx: (3) ! tcx: (3) ! tcx: (4,3) ! tcx: (4,3) ! tcx: (4,3) ! tcx: (4,3) ! tcx: (4,3)
                                         shape = (/ 2,2 /) )
   integer :: stat
   character(200) :: msg

   open ( 1, file = 'array002kl.1', form='formatted', access='sequential' )

   allocate ( b1(3), source = (/ container(4,3)( 101, base(3)('abc') ), container(4,3)( 102, base(3)('def') ), container(4,3)( 103, base(3)('ghi') ) /) ) ! tcx: (3) ! tcx: (3) ! tcx: (3) ! tcx: (4,3) ! tcx: (4,3) ! tcx: (4,3)

   write ( 1, *, iostat = stat, iomsg = msg )             b1((/1,2,3/))
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) )      error stop 101_4

   write ( 1, "(I4,1X,DT)", iostat = stat, iomsg = msg )  b2             !<- reversion occurs
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) )      error stop 2_4

   rewind 1

   deallocate ( b1 )
   allocate (container(4,3):: b1(3) ) ! tcx: container(4,3)

   b2 = container(4,3)(b=base(3)()) ! tcx: (4,3)

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


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 11 changes
! type: container - added parameters (kcontainer_1,lcontainer_1) to invoke with (4,3) / declare with (4,*) - 10 changes
